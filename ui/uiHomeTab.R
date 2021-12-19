uiHomeTab <- bs4TabItem(
    tabName = "home",
    tags$div(
        id = "outer-home",
        tags$div(
            id = "inner-home",
            tags$h1("COVID 19 Tracker", class = "display-2"),
            tags$h3("A Dashboard for Prediction and Control assistance"),
            tags$p("Sponsored by IIM, Visakhapatnam", class = "lead"),
            tags$div(
                class = "d-flex justify-content-center",
                actionBttn("see-more-btn", label = "Go to the Dashboard", style = "jelly", color = "primary"),
                actionBttn("pred-more-btn", label = "Go to the Prediction", style = "jelly", color = "primary")
            )
        ),
        tags$img(src = "IIMV-logo.jpg", class = "rounded-circle", id = "main-logo", width = "200px"),
        HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1440 320"><path fill="#f4f6f9" fill-opacity="1" d="M0,256L48,229.3C96,203,192,149,288,133.3C384,117,480,139,576,170.7C672,203,768,245,864,240C960,235,1056,181,1152,160C1248,139,1344,149,1392,154.7L1440,160L1440,320L1392,320C1344,320,1248,320,1152,320C1056,320,960,320,864,320C768,320,672,320,576,320C480,320,384,320,288,320C192,320,96,320,48,320L0,320Z"></path></svg>')
    ),
    tags$div(
        class = "row",
        tags$div(
            class = "col-md-8 mx-auto text-justify",
            tags$h2("What this Dashboard Offers", class = "home-section-header"),
            tags$p(" 'COVID- 19 Tracker' primarily helps predicting the spread of 
                                   COVID based on a modified extended e-SIR model[1]. 
                                   It analyzes districts and state levels and represents a prediction of 
                                   COVID-19 spread for various scenarios with different possible dates 
                                   of lockdown relaxation, followed by varying degrees of social distancing 
                                   guidelines that may be adopted post-lockdown. 
                                   It also proposes possible strategies to contain the COVID-19 spread 
                                   in each district depending on fraction of the population that will be 
                                   infected at the peak. That can help to manage the medical supply chain 
                                   management.", class = "lead font-italic mb-5"),
            tags$h2("Source of Data", class = "home-section-header"),
            tags$p("The primary source of data are", tags$a("covid19india.org", href = "www.covid19india.org", target = "_blank"),
                   "and their ", tags$a("Github Repository", href = "https://github.com/covid19india/covid19india-react", target = "_blank"),
                   class = "lead font-italic"),
            tags$p("The data and predictions in 'COVID- 19 Tracker' are updated on a daily basis.", class = "lead font-italic"),
            tags$blockquote("'COVID- 19 Tracker' is developed under part of a project that is sponsored by", tags$a("Indian Institute of Management, Visakhapatnam", href = "https://www.iimv.ac.in/"), class = "mb-5"),
            tags$h2("Our Team", class = "home-section-header"),
            tags$ul(
                tags$li(tags$b("Anirban Ghatak"), ", PhD (Former Faculty at IIMV)"),
                tags$li(tags$b("Shivshanker Singh Patel"), ", PhD (Faculty at IIMV)"),
                tags$li(tags$b("Subhrajyoty Roy"), ", (M.Stat. student at ISI Kolkata)"),
                tags$li(tags$b("Soham Bonnerjee"), ", (M.Stat. student at ISI Kolkata)"),
                class = "mb-5"
            ),
            tags$h2("Contact Us", class = "home-section-header"),
            tags$ul(
                tags$li("shivshanker@iimv.ac.in"),
                tags$li("aghatak@gmail.com")),
            tags$p(tags$b("Please write to us if you see any discrepancy or if you have any questions."), class = "mb-5"),
            tags$h2("References", class = "home-section-header"),
            tags$ol(
                tags$li("Song PX, Wang L, Zhou Y, He J, Zhu B, Wang F, Tang L, Eisenberg M (2020) An epidemiological forecast model and software assessing interventions on covid-19 epidemic in china.", tags$a("medRxiv", href = "https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1", target = "_blank"))
            )
        )
    )
)