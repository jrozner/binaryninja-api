# Copyright (c) 2015-2021 Vector 35 Inc
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.


import code
import traceback
import ctypes
import threading
import abc
import sys
import subprocess
from pathlib import Path, PurePath
import re

# Binary Ninja components
import binaryninja
from binaryninja import bncompleter, log
from binaryninja import _binaryninjacore as core
from binaryninja.settings import Settings
from binaryninja.pluginmanager import RepositoryManager
from binaryninja.enums import ScriptingProviderExecuteResult, ScriptingProviderInputReadyState

# 2-3 compatibility
from binaryninja import with_metaclass


class _ThreadActionContext(object):
	_actions = []

	def __init__(self, func):
		self.func = func
		self.interpreter = None
		if hasattr(PythonScriptingInstance._interpreter, "value"):
			self.interpreter = PythonScriptingInstance._interpreter.value
		self.__class__._actions.append(self)
		self.callback = ctypes.CFUNCTYPE(None, ctypes.c_void_p)(lambda ctxt: self.execute())

	def execute(self):
		old_interpreter = None
		if hasattr(PythonScriptingInstance._interpreter, "value"):
			old_interpreter = PythonScriptingInstance._interpreter.value
		PythonScriptingInstance._interpreter.value = self.interpreter
		try:
			self.func()
		finally:
			PythonScriptingInstance._interpreter.value = old_interpreter
			self.__class__._actions.remove(self)


class ScriptingOutputListener(object):
	def _register(self, handle):
		self._cb = core.BNScriptingOutputListener()
		self._cb.context = 0
		self._cb.output = self._cb.output.__class__(self._output)
		self._cb.error = self._cb.error.__class__(self._error)
		self._cb.inputReadyStateChanged = self._cb.inputReadyStateChanged.__class__(self._input_ready_state_changed)
		core.BNRegisterScriptingInstanceOutputListener(handle, self._cb)

	def _unregister(self, handle):
		core.BNUnregisterScriptingInstanceOutputListener(handle, self._cb)

	def _output(self, ctxt, text):
		try:
			self.notify_output(text)
		except:
			log.log_error(traceback.format_exc())

	def _error(self, ctxt, text):
		try:
			self.notify_error(text)
		except:
			log.log_error(traceback.format_exc())

	def _input_ready_state_changed(self, ctxt, state):
		try:
			self.notify_input_ready_state_changed(state)
		except:
			log.log_error(traceback.format_exc())

	def notify_output(self, text):
		pass

	def notify_error(self, text):
		pass

	def notify_input_ready_state_changed(self, state):
		pass


class ScriptingInstance(object):
	def __init__(self, provider, handle = None):
		if handle is None:
			self._cb = core.BNScriptingInstanceCallbacks()
			self._cb.context = 0
			self._cb.destroyInstance = self._cb.destroyInstance.__class__(self._destroy_instance)
			self._cb.executeScriptInput = self._cb.executeScriptInput.__class__(self._execute_script_input)
			self._cb.cancelScriptInput = self._cb.cancelScriptInput.__class__(self._cancel_script_input)
			self._cb.setCurrentBinaryView = self._cb.setCurrentBinaryView.__class__(self._set_current_binary_view)
			self._cb.setCurrentFunction = self._cb.setCurrentFunction.__class__(self._set_current_function)
			self._cb.setCurrentBasicBlock = self._cb.setCurrentBasicBlock.__class__(self._set_current_basic_block)
			self._cb.setCurrentAddress = self._cb.setCurrentAddress.__class__(self._set_current_address)
			self._cb.setCurrentSelection = self._cb.setCurrentSelection.__class__(self._set_current_selection)
			self._cb.completeInput = self._cb.completeInput.__class__(self._complete_input)
			self._cb.completeInput.restype = ctypes.c_void_p
			self.handle = core.BNInitScriptingInstance(provider.handle, self._cb)
			self.delimiters = ' \t\n`~!@#$%^&*()-=+{}\\|;:\'",<>/?'
		else:
			self.handle = core.handle_of_type(handle, core.BNScriptingInstance)
		self.listeners = []

	def __del__(self):
		core.BNFreeScriptingInstance(self.handle)

	def _destroy_instance(self, ctxt):
		try:
			self.perform_destroy_instance()
		except:
			log.log_error(traceback.format_exc())

	def _execute_script_input(self, ctxt, text):
		try:
			return self.perform_execute_script_input(text)
		except:
			log.log_error(traceback.format_exc())
			return ScriptingProviderExecuteResult.InvalidScriptInput

	def _cancel_script_input(self, ctxt):
		try:
			return self.perform_cancel_script_input()
		except:
			log.log_error(traceback.format_exc())
			return ScriptingProviderExecuteResult.ScriptExecutionCancelled

	def _set_current_binary_view(self, ctxt, view):
		try:
			if view:
				view = binaryninja.binaryview.BinaryView(handle = core.BNNewViewReference(view))
			else:
				view = None
			self.perform_set_current_binary_view(view)
		except:
			log.log_error(traceback.format_exc())

	def _set_current_function(self, ctxt, func):
		try:
			if func:
				func = binaryninja.function.Function(handle = core.BNNewFunctionReference(func))
			else:
				func = None
			self.perform_set_current_function(func)
		except:
			log.log_error(traceback.format_exc())

	def _set_current_basic_block(self, ctxt, block):
		try:
			if block:
				func = core.BNGetBasicBlockFunction(block)
				if func is None:
					block = None
				else:
					block = binaryninja.basicblock.BasicBlock(core.BNNewBasicBlockReference(block),
						binaryninja.binaryview.BinaryView(handle = core.BNGetFunctionData(func)))
					core.BNFreeFunction(func)
			else:
				block = None
			self.perform_set_current_basic_block(block)
		except:
			log.log_error(traceback.format_exc())

	def _set_current_address(self, ctxt, addr):
		try:
			self.perform_set_current_address(addr)
		except:
			log.log_error(traceback.format_exc())

	def _set_current_selection(self, ctxt, begin, end):
		try:
			self.perform_set_current_selection(begin, end)
		except:
			log.log_error(traceback.format_exc())

	def _complete_input(self, ctxt, text, state):
		try:
			if not isinstance(text, str):
				text = text.decode("charmap")
			return ctypes.cast(binaryninja.cstr(self.perform_complete_input(text, state)), ctypes.c_void_p).value
		except:
			log.log_error(traceback.format_exc())
			return ctypes.cast(binaryninja.cstr(""), ctypes.c_void_p).value

	@abc.abstractmethod
	def perform_destroy_instance(self):
		raise NotImplementedError

	@abc.abstractmethod
	def perform_execute_script_input(self, text):
		return ScriptingProviderExecuteResult.InvalidScriptInput

	@abc.abstractmethod
	def perform_cancel_script_input(self):
		return ScriptingProviderExecuteResult.ScriptExecutionCancelled

	@abc.abstractmethod
	def perform_set_current_binary_view(self, view):
		raise NotImplementedError

	@abc.abstractmethod
	def perform_set_current_function(self, func):
		raise NotImplementedError

	@abc.abstractmethod
	def perform_set_current_basic_block(self, block):
		raise NotImplementedError

	@abc.abstractmethod
	def perform_set_current_address(self, addr):
		raise NotImplementedError

	@abc.abstractmethod
	def perform_set_current_selection(self, begin, end):
		raise NotImplementedError

	@abc.abstractmethod
	def perform_complete_input(self, text, state):
		raise NotImplementedError

	@property
	def input_ready_state(self):
		return core.BNGetScriptingInstanceInputReadyState(self.handle)

	@input_ready_state.setter
	def input_ready_state(self, value):
		core.BNNotifyInputReadyStateForScriptingInstance(self.handle, value.value)

	def output(self, text):
		core.BNNotifyOutputForScriptingInstance(self.handle, text)

	def error(self, text):
		core.BNNotifyErrorForScriptingInstance(self.handle, text)

	def execute_script_input(self, text):
		return core.BNExecuteScriptInput(self.handle, text)

	def cancel_script_input(self, text):
		return core.BNCancelScriptInput(self.handle)

	def set_current_binary_view(self, view):
		if view is not None:
			view = view.handle
		core.BNSetScriptingInstanceCurrentBinaryView(self.handle, view)

	def set_current_function(self, func):
		if func is not None:
			func = func.handle
		core.BNSetScriptingInstanceCurrentFunction(self.handle, func)

	def set_current_basic_block(self, block):
		if block is not None:
			block = block.handle
		core.BNSetScriptingInstanceCurrentBasicBlock(self.handle, block)

	def set_current_address(self, addr):
		core.BNSetScriptingInstanceCurrentAddress(self.handle, addr)

	def set_current_selection(self, begin, end):
		core.BNSetScriptingInstanceCurrentSelection(self.handle, begin, end)

	def complete_input(self, text, state):
		return core.BNScriptingInstanceCompleteInput(self.handle, text, state)

	def register_output_listener(self, listener):
		listener._register(self.handle)
		self.listeners.append(listener)

	def unregister_output_listener(self, listener):
		if listener in self.listeners:
			listener._unregister(self.handle)
			self.listeners.remove(listener)

	@property
	def delimiters(self):
		return core.BNGetScriptingInstanceDelimiters(self.handle)

	@delimiters.setter
	def delimiters(self, value):
		core.BNSetScriptingInstanceDelimiters(self.handle, value)


class _ScriptingProviderMetaclass(type):

	@property
	def list(self):
		"""List all ScriptingProvider types (read-only)"""
		binaryninja._init_plugins()
		count = ctypes.c_ulonglong()
		types = core.BNGetScriptingProviderList(count)
		result = []
		for i in range(0, count.value):
			result.append(ScriptingProvider(types[i]))
		core.BNFreeScriptingProviderList(types)
		return result

	def __iter__(self):
		binaryninja._init_plugins()
		count = ctypes.c_ulonglong()
		types = core.BNGetScriptingProviderList(count)
		try:
			for i in range(0, count.value):
				yield ScriptingProvider(types[i])
		finally:
			core.BNFreeScriptingProviderList(types)

	def __getitem__(self, value):
		binaryninja._init_plugins()
		provider = core.BNGetScriptingProviderByName(str(value))
		if provider is None:
			raise KeyError("'%s' is not a valid scripting provider" % str(value))
		return ScriptingProvider(provider)

	def __setattr__(self, name, value):
		try:
			type.__setattr__(self, name, value)
		except AttributeError:
			raise AttributeError("attribute '%s' is read only" % name)


class ScriptingProvider(with_metaclass(_ScriptingProviderMetaclass, object)):
	_registered_providers = []

	def __init__(self, handle = None):
		if handle is not None:
			self.handle = core.handle_of_type(handle, core.BNScriptingProvider)
			self.__dict__["name"] = core.BNGetScriptingProviderName(handle)

	@property
	def name(self):
		return NotImplemented

	@property
	def instance_class(self):
		return NotImplemented

	@property
	def list(self):
		"""Allow tab completion to discover metaclass list property"""
		pass

	def register(self):
		self._cb = core.BNScriptingProviderCallbacks()
		self._cb.context = 0
		self._cb.createInstance = self._cb.createInstance.__class__(self._create_instance)
		self._cb.loadModule = self._cb.loadModule.__class__(self._load_module)
		self._cb.installModules = self._cb.installModules.__class__(self._install_modules)
		self.handle = core.BNRegisterScriptingProvider(self.__class__.name, self.__class__.apiName, self._cb)
		self.__class__._registered_providers.append(self)

	def _create_instance(self, ctxt):
		try:
			result = self.__class__.instance_class(self)
			if result is None:
				return None
			return ctypes.cast(core.BNNewScriptingInstanceReference(result.handle), ctypes.c_void_p).value
		except:
			log.log_error(traceback.format_exc())
			return None

	def create_instance(self):
		result = core.BNCreateScriptingProviderInstance(self.handle)
		if result is None:
			return None
		return ScriptingInstance(self, handle = result)

	def _load_plugin(self, ctx, repo_path, plugin_path, force):
		return False

	def _install_modules(self, ctx, modules):
		return False

class _PythonScriptingInstanceOutput(object):
	def __init__(self, orig, is_error):
		self.orig = orig
		self.is_error = is_error
		self.buffer = ""
		self.encoding = 'UTF-8'
		self.errors = None
		self.mode = 'w'
		self.name = 'PythonScriptingInstanceOutput'
		self.newlines = None

	def close(self):
		pass

	def closed(self):
		return False

	def flush(self):
		pass

	def isatty(self):
		return False

	def next(self):
		raise IOError("File not open for reading")

	def read(self):
		raise IOError("File not open for reading")

	def readinto(self):
		raise IOError("File not open for reading")

	def readlines(self):
		raise IOError("File not open for reading")

	def seek(self):
		pass

	def sofspace(self):
		return 0

	def truncate(self):
		pass

	def tell(self):
		return self.orig.tell()

	def writelines(self, lines):
		return self.write('\n'.join(lines))

	def write(self, data):
		interpreter = None
		if hasattr(PythonScriptingInstance._interpreter, "value"):
			interpreter = PythonScriptingInstance._interpreter.value

		if interpreter is None:
			if log.is_output_redirected_to_log():
				self.buffer += data
				while True:
					i = self.buffer.find('\n')
					if i == -1:
						break
					line = self.buffer[:i]
					self.buffer = self.buffer[i + 1:]

					if self.is_error:
						log.log_error(line)
					else:
						log.log_info(line)
			else:
				self.orig.write(data)
		else:
			PythonScriptingInstance._interpreter.value = None
			try:
				if self.is_error:
					interpreter.instance.error(data)
				else:
					interpreter.instance.output(data)
			finally:
				PythonScriptingInstance._interpreter.value = interpreter


class _PythonScriptingInstanceInput(object):
	def __init__(self, orig):
		self.orig = orig

	def isatty(self):
		return False

	def read(self, size):
		interpreter = None
		if hasattr(PythonScriptingInstance._interpreter, "value"):
			interpreter = PythonScriptingInstance._interpreter.value

		if interpreter is None:
			return self.orig.read(size)
		else:
			PythonScriptingInstance._interpreter.value = None
			try:
				result = interpreter.read(size)
			finally:
				PythonScriptingInstance._interpreter.value = interpreter
			return result

	def readline(self):
		interpreter = None
		if hasattr(PythonScriptingInstance._interpreter, "value"):
			interpreter = PythonScriptingInstance._interpreter.value

		if interpreter is None:
			return self.orig.readline()
		else:
			result = ""
			while True:
				data = interpreter.read(1)
				result += data
				if (len(data) == 0) or (data == "\n"):
					break
			return result


class BlacklistedDict(dict):

	def __init__(self, blacklist, *args):
		super(BlacklistedDict, self).__init__(*args)
		self.__blacklist = set(blacklist)
		self._blacklist_enabled = True

	def __setitem__(self, k, v):
		if self.blacklist_enabled and k in self.__blacklist:
			log.log_warn('Setting variable "{}" will have no affect as it is automatically controlled by the ScriptingProvider.'.format(k))
		super(BlacklistedDict, self).__setitem__(k, v)

	def enable_blacklist(self, enabled):
		self.__enable_blacklist = enabled

	@property
	def blacklist_enabled(self):
		return self._blacklist_enabled

	@blacklist_enabled.setter
	def blacklist_enabled(self, value):
		self._blacklist_enabled = value


def bninspect(code, globals_, locals_):
	"""
	``bninspect`` prints documentation about a command that is about to be run
	The interpreter will invoke this function if you input a line ending in `?` e.g. `bv?`

	:param str code: Python code to be evaluated
	:param dict globals_: globals() from callsite
	:param dict locals_: locals() from callsite
	"""
	try:
		import inspect
		value = eval(code, globals_, locals_)
		doc = inspect.getdoc(value)
		if doc is None:
			comments = inspect.getcomments(value)
			if comments is None:
				print(f"No documentation found for {code}")
			else:
				print(comments)
		else:
			print(doc)
	except:
		# Hide exceptions so the normal execution can report them
		pass


class PythonScriptingInstance(ScriptingInstance):
	_interpreter = threading.local()

	class InterpreterThread(threading.Thread):
		def __init__(self, instance):
			super(PythonScriptingInstance.InterpreterThread, self).__init__()
			self.instance = instance
			# Note: "current_address" and "here" are interactive auto-variables (i.e. can be set by user and programmatically)
			blacklisted_vars = {"current_view", "bv", "current_function", "current_basic_block", "current_selection", "current_llil", "current_mlil", "current_hlil"}
			self.locals = BlacklistedDict(blacklisted_vars, {"__name__": "__console__", "__doc__": None, "binaryninja": sys.modules[__name__]})
			self.interpreter = code.InteractiveConsole(self.locals)
			self.event = threading.Event()
			self.daemon = True

			# Latest selections from UI
			self.current_view = None
			self.current_func = None
			self.current_block = None
			self.current_addr = 0
			self.current_selection_begin = 0
			self.current_selection_end = 0

			# Selections that were current as of last issued command
			self.active_view = None
			self.active_func = None
			self.active_block = None
			self.active_addr = 0
			self.active_selection_begin = 0
			self.active_selection_end = 0

			self.locals["get_selected_data"] = self.get_selected_data
			self.locals["write_at_cursor"] = self.write_at_cursor

			self.exit = False
			self.code = None
			self.input = ""

			self.completer = bncompleter.Completer(namespace = self.locals)

			self.interpreter.push("from binaryninja import *")

		def execute(self, code):
			self.code = code
			self.event.set()

		def add_input(self, data):
			self.input += data
			self.event.set()

		def end(self):
			self.exit = True
			self.event.set()

		def read(self, size):
			while not self.exit:
				if len(self.input) > size:
					result = self.input[:size]
					self.input = self.input[size:]
					return result
				elif len(self.input) > 0:
					result = self.input
					self.input = ""
					return result
				self.instance.input_ready_state = ScriptingProviderInputReadyState.ReadyForScriptProgramInput
				self.event.wait()
				self.event.clear()
			return ""

		def run(self):
			while not self.exit:
				self.event.wait()
				self.event.clear()
				if self.exit:
					break
				if self.code is not None:
					self.instance.input_ready_state = ScriptingProviderInputReadyState.NotReadyForInput
					code = self.code
					self.code = None

					PythonScriptingInstance._interpreter.value = self
					try:
						self.update_locals()

						# If a single-line command ends in ?, show docs as well
						if code[-2:] == b'?\n' and len(code.split(b'\n')) < 3:
							escaped_code = repr(code[:-2])
							self.interpreter.push(f'bninspect({escaped_code}, globals(), locals())\n')
							# Strip ? from the evaluated input
							code = code[:-2] + b'\n'

						for line in code.split(b'\n'):
							self.interpreter.push(line.decode('charmap'))

						tryNavigate = True
						if isinstance(self.locals["here"], str) or isinstance(self.locals["current_address"], str):
							try:
								self.locals["here"] = self.active_view.parse_expression(self.locals["here"], self.active_addr)
							except ValueError as e:
								sys.stderr.write(e)
								tryNavigate = False
						if tryNavigate:
							if self.locals["here"] != self.active_addr:
								if not self.active_view.file.navigate(self.active_view.file.view, self.locals["here"]):
									sys.stderr.write("Address 0x%x is not valid for the current view\n" % self.locals["here"])
							elif self.locals["current_address"] != self.active_addr:
								if not self.active_view.file.navigate(self.active_view.file.view, self.locals["current_address"]):
									sys.stderr.write("Address 0x%x is not valid for the current view\n" % self.locals["current_address"])
						if self.active_view is not None:
							self.active_view.update_analysis()
					except:
						traceback.print_exc()
					finally:
						PythonScriptingInstance._interpreter.value = None
						self.instance.input_ready_state = ScriptingProviderInputReadyState.ReadyForScriptExecution

		def update_locals(self):
			self.active_view = self.current_view
			self.active_func = self.current_func
			self.active_block = self.current_block
			self.active_addr = self.current_addr
			self.active_selection_begin = self.current_selection_begin
			self.active_selection_end = self.current_selection_end

			self.locals.blacklist_enabled = False
			self.locals["current_view"] = self.active_view
			self.locals["bv"] = self.active_view
			self.locals["current_function"] = self.active_func
			self.locals["current_basic_block"] = self.active_block
			self.locals["current_address"] = self.active_addr
			self.locals["here"] = self.active_addr
			self.locals["current_selection"] = (self.active_selection_begin, self.active_selection_end)
			if self.active_func is None:
				self.locals["current_llil"] = None
				self.locals["current_mlil"] = None
				self.locals["current_hlil"] = None
			else:
				self.locals["current_llil"] = self.active_func.llil
				self.locals["current_mlil"] = self.active_func.mlil
				self.locals["current_hlil"] = self.active_func.hlil
			self.locals.blacklist_enabled = True


		def get_selected_data(self):
			if self.active_view is None:
				return None
			length = self.active_selection_end - self.active_selection_begin
			return self.active_view.read(self.active_selection_begin, length)

		def write_at_cursor(self, data):
			if self.active_view is None:
				return 0
			selected_length = self.active_selection_end - self.active_selection_begin
			if (len(data) == selected_length) or (selected_length == 0):
				return self.active_view.write(self.active_selection_begin, data)
			else:
				self.active_view.remove(self.active_selection_begin, selected_length)
				return self.active_view.insert(self.active_selection_begin, data)

	def __init__(self, provider):
		super(PythonScriptingInstance, self).__init__(provider)
		self.interpreter = PythonScriptingInstance.InterpreterThread(self)
		self.interpreter.start()
		self.queued_input = ""
		self.input_ready_state = ScriptingProviderInputReadyState.ReadyForScriptExecution

	@abc.abstractmethod
	def perform_destroy_instance(self):
		self.interpreter.end()

	@abc.abstractmethod
	def perform_execute_script_input(self, text):
		if self.input_ready_state == ScriptingProviderInputReadyState.NotReadyForInput:
			return ScriptingProviderExecuteResult.InvalidScriptInput

		if self.input_ready_state == ScriptingProviderInputReadyState.ReadyForScriptProgramInput:
			if len(text) == 0:
				return ScriptingProviderExecuteResult.SuccessfulScriptExecution
			self.input_ready_state = ScriptingProviderInputReadyState.NotReadyForInput
			self.interpreter.add_input(text)
			return ScriptingProviderExecuteResult.SuccessfulScriptExecution

		try:
			if isinstance(text, str):
				result = code.compile_command(text)
			else:
				result = code.compile_command(text.decode("charmap"))
		except:
			result = False

		if result is None:
			# Command is not complete, ask for more input
			return ScriptingProviderExecuteResult.IncompleteScriptInput

		self.input_ready_state = ScriptingProviderInputReadyState.NotReadyForInput
		self.interpreter.execute(text)
		return ScriptingProviderExecuteResult.SuccessfulScriptExecution

	@abc.abstractmethod
	def perform_cancel_script_input(self):
		for tid, tobj in threading._active.items():
			if tobj is self.interpreter:
				if ctypes.pythonapi.PyThreadState_SetAsyncExc(ctypes.c_long(tid), ctypes.py_object(KeyboardInterrupt)) != 1:
					ctypes.pythonapi.PyThreadState_SetAsyncExc(ctypes.c_long(tid), None)
				break

	@abc.abstractmethod
	def perform_set_current_binary_view(self, view):
		self.interpreter.current_view = view

	@abc.abstractmethod
	def perform_set_current_function(self, func):
		self.interpreter.current_func = func

	@abc.abstractmethod
	def perform_set_current_basic_block(self, block):
		self.interpreter.current_block = block

	@abc.abstractmethod
	def perform_set_current_address(self, addr):
		self.interpreter.current_addr = addr

	@abc.abstractmethod
	def perform_set_current_selection(self, begin, end):
		self.interpreter.current_selection_begin = begin
		self.interpreter.current_selection_end = end

	@abc.abstractmethod
	def perform_complete_input(self, text, state):
		self.interpreter.update_locals()
		result = self.interpreter.completer.complete(text, state)
		if result is None:
			return ""
		return result


def python_binary_for_library(lib_path: str) -> str:
	import platform
	core_platform = platform.system()
	if core_platform == "Darwin":
		if (lib_path.startswith("/usr/local/Cellar/python") or
			lib_path.startsWith("/usr/local/Frameworks/Python.framework") or
			lib_path.startsWith("/Library/Developer/CommandLineTools/Library/Frameworks/Python3.framework/Versions")):
			path = Path(lib_path).parent / "bin"
			if not path.is_dir():
				return None
			bin_regex = ("^python((@)?\\d*(.\\d+)*)?$")
			for entry in path.glob("python*"):
				if re.search(bin_regex, entry.name):
					return str(entry)
		# TODO: Bundled python
	elif core_platform == "Linux":
		lib_regex = "^libpython(\\d+(.\\d+)*)+(m)?\\.so(.\\d+(.\\d+)*)?$"
		path = Path(lib_path)
		result = re.search(lib_regex, path.name)
		if result is None:
			return None
		python_bin = "python" + result.group(1)
		if lib_path.startswith("/usr/lib/x86_64-linux-gnu"):
			path /= ".." / ".." / "bin" / python_bin
		elif lib_path.startswith("/usr/local/lib") or lib_path.startswith("/usr/lib"):
			path /= ".." / "bin" / python_bin
		else:
			return None

		return str(path)

	elif (core_platform == "Windows") or (core_platform.find("CYGWIN_NT") == 0):
		bin_regex = "^python((@)?\\d*(.\\d+)*)?.exe$"
		path = Path(lib_path)
		for entry in path.glob("*.exe"):
			if entry.is_file() and re.search(bin_regex, entry.name):
					return str(entry)
	return None

class PythonScriptingProvider(ScriptingProvider):
	name = "Python"
	apiName = f"python{sys.version_info.major}" # Used for plugin compatibility testing
	instance_class = PythonScriptingInstance

	def _load_module(self, ctx, repo_path, module, force):
		try:
			repo_path = repo_path.decode("utf-8")
			module = module.decode("utf-8")
			repo = RepositoryManager()[repo_path]
			plugin = repo[module]

			if not force and self.name not in plugin.api:
				raise ValueError(f"Plugin API name is not {self.name}")

			if not force and core.core_platform not in plugin.install_platforms:
				raise ValueError(f"Current platform {core.core_platform} isn't in list of "\
					f"valid platforms for this plugin {plugin.install_platforms}")
			if not plugin.installed:
				plugin.installed = True

			plugin_full_path = Path(repo.full_path) / plugin.path
			if repo.full_path not in sys.path:
				sys.path.append(repo.full_path)
			if plugin_full_path not in sys.path:
				sys.path.append(plugin_full_path)

			__import__(module)
			return True
		except KeyError:
			log.log_error(f"Failed to find python plugin: {repo_path}/{module}")
		except ImportError as ie:
			log.log_error(f"Failed to import python plugin: {repo_path}/{module}: {ie}")
		return False

	def _install_modules(self, ctx, module):
		settings = Settings()
		virtualEnv = settings.get_string("python.virtualenv")
		interpreter_binary = settings.get_string("python.interpreterBinary")
		if virtualEnv != "":
			path = Path(virtualEnv) / "bin" / "python"
			if not path.is_file():
				log.log_error(f"Virtual environment not configured properly path {path} is not a file")
				return False
		elif interpreter_binary != "":
			path = Path(interpreter_binary)
			if not path.is_file():
				log.log_error(f"Python interpreter binary setting not configured properly path {path} is not a file")
				return False
		else:
			interpreter_library = settings.get_string("python.interpreter")
			bin_path = python_binary_for_library(interpreter_library)
			if bin_path is None:
				log.log_error(f"Failed to discover python binary for configured library {interpreter_library}."\
					f" Please specify the corresponding python binary in the setting 'Python Interpreter Binary'")
				return False
			path = Path(bin_path)

		result = True
		for dependency in module.decode("utf-8").split("\n"):
			if len(dependency) == 0:
				continue

			#TODO: Attempt to parse requirements string and notify on bad format
			try:
				subprocess.check_call([str(path), "-m", "pip", "install", dependency])
				log.log_info(f"Installed dependency: {dependency}")
			except Exception as e:
				log.log_info(f"Failed to install dependency {dependency} {e}")
				result = False
		return result


PythonScriptingProvider().register()
# Wrap stdin/stdout/stderr for Python scripting provider implementation
original_stdin = sys.stdin
original_stdout = sys.stdout
original_stderr = sys.stderr

def redirect_stdio():
	sys.stdin = _PythonScriptingInstanceInput(sys.stdin)
	sys.stdout = _PythonScriptingInstanceOutput(sys.stdout, False)
	sys.stderr = _PythonScriptingInstanceOutput(sys.stderr, True)
	sys.excepthook = sys.__excepthook__
