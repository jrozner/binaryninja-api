#pragma once

#include <QtCore/QPointer>
#include <QtCore/QTimer>
#include <QtWidgets/QLabel>
#include <QtWidgets/QMenu>
#include <QtWidgets/QWidget>
#include <functional>
#include <map>
#include <string>
#include "binaryninjaapi.h"
#include "action.h"
#include "viewframe.h"

class BINARYNINJAUIAPI ContextMenuManager
{
	QWidget* m_parent;
	QPointer<QMenu> m_menu;
	MenuInstance* m_instance;

public:
	ContextMenuManager(): m_parent(nullptr), m_menu(nullptr), m_instance(nullptr) { }
	ContextMenuManager(QWidget* parent);
	~ContextMenuManager();
	QMenu* create();
	MenuInstance* show(View* view);
	MenuInstance* show(Menu* source, UIActionHandler* handler);
	bool isActive() { return !m_menu.isNull(); }
};


class BINARYNINJAUIAPI MenuHelper: public QLabel
{
	Q_OBJECT

	QPalette::ColorRole m_backgroundRole;
	QPalette::ColorRole m_activeBackgroundRole;
	QPalette::ColorRole m_pressedBackgroundRole;
	QPalette::ColorRole m_foregroundRole;
	QPalette::ColorRole m_activeForegroundRole;
	QPalette::ColorRole m_pressedForegroundRole;

protected:
	Menu m_menu;
	ContextMenuManager m_contextMenuManager;
	QTimer* m_timer;
	bool m_activeOnHover = true;
	bool m_active;
	bool m_pressed;

public:
	MenuHelper() { }
	MenuHelper(QWidget* parent, bool activeOnHover = true);

	void setBackgroundColorRole(QPalette::ColorRole role);
	void setActiveBackgroundColorRole(QPalette::ColorRole role);
	void setPressedBackgroundColorRole(QPalette::ColorRole role);
	void setForegroundColorRole(QPalette::ColorRole role);
	void setActiveForegroundColorRole(QPalette::ColorRole role);
	void setPressedForegroundColorRole(QPalette::ColorRole role);

Q_SIGNALS:
	void clicked();

protected Q_SLOTS:
	virtual void showMenu() = 0;

private Q_SLOTS:
	void underMouseTimerEvent();

protected:
	void enterEvent(QEvent* event) override;
	void leaveEvent(QEvent* event) override;
	void mousePressEvent(QMouseEvent* event) override;
	void mouseReleaseEvent(QMouseEvent* event) override;

	void updateColors();
};
