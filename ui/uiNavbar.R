uiNavbar <- bs4DashSidebar(
    expand_on_hover = TRUE,
    fixed = TRUE,
    skin = "light",
    status = "primary",
    title = "COVID 19 Tracker",
    brandColor = "dark",
    url = "",
    src = "IIMV-logo.jpg",
    opacity = 1,
    bs4SidebarMenu(
        id = "mainMenu",
        flat = FALSE,
        compact = FALSE,
        child_indent = TRUE,
        bs4SidebarMenuItem(
            "Home",
            tabName = "home",
            icon = "home"
        ),
        bs4SidebarMenuItem(
            "Summary",
            tabName = "summary",
            icon = "info-circle"
        ),
        bs4SidebarMenuItem(
            "Statewise Counts",
            icon = "chart-bar",
            startExpanded = TRUE,
            bs4SidebarMenuSubItem(
                "Confirmed Cases",
                tabName = "confirmed",
                icon = "heartbeat"
            ),
            bs4SidebarMenuSubItem(
                "Active Cases",
                tabName = "active",
                icon = "bed"
            ),
            bs4SidebarMenuSubItem(
                "Recovered Cases",
                tabName = "recovered",
                icon = "medkit"
            ),
            bs4SidebarMenuSubItem(
                "Death Cases",
                tabName = "deaths",
                icon = "skull"
            )
        ),
        bs4SidebarMenuItem(
            "Reproduction Rate",
            tabName = "R0",
            icon = "users"
        ),
        bs4SidebarMenuItem(
            "Predictive Modelling",
            tabName = "prediction",
            icon = "globe"
        )
        
    )
)