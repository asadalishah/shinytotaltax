# tax insights

library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(hrbrthemes)
library(lubridate)
library(plotly)
library(gt)

# read data
temp <- read.table("ptax_upto2018.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# converting house to factor
temp$House <- factor(temp$House, levels = c("Senate", 
                                            "National Assembly", 
                                            "Balochistan Assembly", 
                                            "KPK Assembly",
                                            "Punjab Assembly",
                                            "Sindh Assembly"))

# year to well year
temp$Year <- year(ymd(as.Date(ISOdate(temp$Year, 6, 30))))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Parliamentarians' Tax Directory Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "house",
                               label = h3("Choose house(s)"),
                               choices = list("Senate" = "Senate",
                                              "National Assembly" = "National Assembly",
                                              "Balochistan Assembly" = "Balochistan Assembly",
                                              "KPK Assembly" = "KPK Assembly",
                                              "Punjab Assembly" = "Punjab Assembly",
                                              "Sindh Assembly" = "Sindh Assembly"),
                               selected = "Senate")),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel("Total Income Tax", plotlyOutput("plot1")),
            tabPanel("Yearly Change", plotlyOutput("plot2")),
            tabPanel("Zero Income Tax Payers", plotlyOutput("plot3")),
            tabPanel("Names of Zero Taxer", tableOutput("tab3")),
            # tabPanel("Income Tax Distribution", plotlyOutput("plot4")), # distribution
            tabPanel("Income Tax Distribution", plotOutput("plot_tst")), # distribution
            tabPanel("Contribution Ratio", plotlyOutput("plot5")), # contribution
            tabPanel("Top-10 Tax Payers", tableOutput("tab2")), # numbers coming out strangely
            tabPanel("Top-10 Contributions", plotlyOutput("plot6")),
            tabPanel("Income Tax Data by House", tableOutput("tab1")),
            tabPanel("Summary Statistics", tableOutput("tab4"))
            # tabPanel("Contributoin", plotOutput("plot5"))
        )
    )
))

# Define server logic 
server <- function(input, output) {

    output$plot1 <- renderPlotly({
        chosenVal <- paste(input$house)
        total <- temp %>%
            filter(House %in% chosenVal) %>%
            group_by(Year, House) %>%
            summarize(Total = sum(IncomeTax)/1000000) %>%
            select(Year, House, Total)

        plot1 <- ggplot(total, aes(fill=House, x=Year, y=Total)) +
            geom_bar(position="stack", stat = "identity") +
            scale_y_continuous() +
            labs(x="Year", y="Total Income Tax (Million PKR)",
                 title="Total Income Tax Paid by All the Parlimentarians of the House(s)",
                 subtitle="Million Rupees",
                 caption="Source: FBR's Parliamentarians' Tax Directory") +
            theme_ipsum_rc(grid="Y")
        
        plot1 <- ggplotly(plot1)
        plot1
        
        })
    
    # change
    output$plot2 <- renderPlotly({
        chosenVal <- paste(input$house)
        
        change <- temp %>% 
            filter(House %in% chosenVal) %>% 
            group_by(House, Year) %>%
            summarize(Total = sum(IncomeTax)/1000000) %>% 
            mutate(Change = Total / lag(Total) * 100) %>% 
            mutate(Change = ifelse(is.na(Change), 100, Change))
        
        plot2 <- ggplot(change, aes(x = Year, y = Change)) + 
            geom_smooth(aes(colour = House)) +
            geom_point(size = 2) + 
            scale_colour_viridis_d(name = "") +
            theme_ipsum_rc() + 
            theme(legend.position = "top", legend.text=element_text(size=15)) +
            ggtitle("Percentage Change in Overall Income Tax Paid", subtitle = "% Change; Year 1 Indexed to 100") +
            labs(x = "Years", y = "% Change from Previous Year", 
                 caption = "Source: FBR's Parliamentarians' Tax Directory") + 
            scale_y_continuous(labels = scales::comma)
        
        plot2 <- ggplotly(plot2)
        plot2
    })
    
    # top-10 tax
    output$plot6 <- renderPlotly({
        chosenVal <- paste(input$house)
        
        t10tax <- temp %>% 
            filter(House %in% chosenVal) %>% 
            group_by(House, Year) %>%
            mutate(propIncomeTax = IncomeTax / sum(IncomeTax)) %>% 
            arrange(-IncomeTax) %>% 
            select(House, Year, Name, IncomeTax, propIncomeTax) %>% 
            top_n(., 10, IncomeTax) %>% 
            summarize(top10abs = sum(IncomeTax), top10prop = sum(propIncomeTax)*100)
        
        plot6 <- ggplot(t10tax, aes(x = Year, y = top10prop)) + 
            geom_smooth(aes(colour = House)) +
            geom_point(size = 2) + 
            scale_colour_viridis_d(name = "") +
            theme_ipsum_rc() + 
            theme(legend.position = "top", legend.text=element_text(size=15)) +
            ggtitle("Percentage of Total Income Tax Paid by Top-10 Tax Payers  of the House", subtitle = "% Change; Year 1 Indexed to 100") +
            labs(x = "Years", y = "% of Total Income Tax", 
                 caption = "Source: FBR's Parliamentarians' Tax Directory") + 
            scale_y_continuous(limits=c(0, 100), labels = scales::comma)
        
        plot6 <- ggplotly(plot6)
        plot6
    })
    
    # zero tax plot 3
    output$plot3 <- renderPlotly({
        chosenVal <- paste(input$house)
        zero <- temp %>% 
            filter(House %in% chosenVal) %>% 
            filter(IncomeTax == 0) %>% 
            group_by(Year, House) %>%
            tally(., NULL)
        
        plot3 <- ggplot(zero, aes(fill=House, x=Year, y=n)) +
            geom_bar(position="stack", stat = "identity") +
            scale_y_continuous() +
            labs(x="Year", y="# Zero Income Tax Payers",
                 title="Number of Parlimentarians Paying Zero Income Tax",
                 subtitle="Number of Parlimentarians Paying Zero Income Tax",
                 caption="Source: FBR's Parliamentarians' Tax Directory") + 
            theme_ipsum_rc(grid="Y")
        
        plot3 <- ggplotly(plot3)
        plot3
    })
    
    # distributoin
    output$plot4 <- renderPlotly({
        chosenVal <- paste(input$house)
        dist <- temp %>% 
            filter(House %in% chosenVal, Year == 2018)
        
        x <- dist %>% top_n(., 10, IncomeTax)
        
        ggplot(data = dist, aes(x = reorder(Name, IncomeTax),
                                y = IncomeTax/1000000,
                                label = Name)) +
            geom_col(color = "slategray1", fill="steelblue1") +
            labs(x="Individual Assembly Members", y="Income Tax Paid (Million PKR)",
                 title="Income Tax Paid by Each Assembly Member in 2018",
                 subtitle="Million Rupees",
                 caption="Source: FBR's Parliamentarians' 2018 Tax Directory") + 
            theme(axis.text.x = element_blank()) +
            theme_ipsum_rc() +
            ggrepel::geom_label_repel(data = x, aes(label = Name),
                                      box.padding   = 0.1, 
                                      point.padding = 0.5,
                                      segment.color = 'grey50')
            
        plot4 <- ggplotly(plot4)
        plot4
    })

    # distributoin
    output$plot_tst <- renderPlot({
        chosenVal <- paste(input$house)
        dist <- temp %>% 
            filter(House %in% chosenVal, Year == 2018)
        
        x <- dist %>% top_n(., 10, IncomeTax)
        
        ggplot(data = dist, aes(x = reorder(Name, IncomeTax),
                                y = IncomeTax/1000000,
                                label = Name)) +
            geom_col(color = "slategray1", fill="steelblue1") +
            labs(x="Individual Assembly Members", y="Income Tax Paid (Million PKR)",
                 title="Income Tax Paid by Each Assembly Member in 2018",
                 subtitle="Million Rupees",
                 caption="Source: FBR's Parliamentarians' 2018 Tax Directory") + 
            theme(axis.text.x = element_blank()) +
            theme_ipsum_rc() +
            ggrepel::geom_label_repel(data = x, aes(label = Name),
                                      box.padding   = 0.1, 
                                      point.padding = 0.5,
                                      segment.color = 'grey50') +
            theme(axis.text.x = element_blank())
        
    })
    
    # contribution
    # how to add a summary - total tax, total members, mean, median, max
    output$plot5 <- renderPlotly({
        chosenVal <- paste(input$house)
        contrib <- temp %>% 
            filter(House %in% chosenVal, Year == max(as.numeric(Year))) %>% 
            mutate(propTax = IncomeTax/sum(IncomeTax)*100) %>%
            arrange(-propTax) %>% 
            mutate(cumSumTax = cumsum(propTax)) %>% 
            mutate(propMember = 1/n()*100) %>%
            mutate(cumSumMember = cumsum(propMember)) %>% 
            select(Name, cumSumTax, cumSumMember, IncomeTax)
        
        plot5 <- ggplot(contrib, aes(x=cumSumMember, y=cumSumTax)) +
            geom_line(color = "steelblue1", size = 1.2) +
            labs(x="% Members of the House(s)", y="% of Total Income Tax Paid by the House(s)",
                 title="Income Tax Contributions Ratio for 2018",
                 subtitle="% Contributions to Income Tax",
                 caption="Source: FBR's Parliamentarians' 2018 Tax Directory") + 
            scale_x_continuous(limits=c(-5, 100), labels = scales::comma) +
            scale_y_continuous(limits=c(-5, 100), labels = scales::comma) +
            theme_ipsum_rc(grid="XY")
    
        plot5 <- ggplotly(plot5)
        plot5
    })
    
    # tax data
    output$tab1 <- renderTable({
        chosenVal <- paste(input$house)
        total <- temp %>% 
            filter(House %in% chosenVal) %>% 
            #filter(House %in% c(input$house)) %>% 
            group_by(House, Year) %>% 
            summarize(TotalMillionPKR = sum(IncomeTax)/1000000) %>% 
            select(Year, House, TotalMillionPKR)
        
        total %>% 
            gt() %>% 
            tab_header(
                title = "Income Tax Paid",
                subtitle = "Million Rupees"
            )
    })
    
    # top-n
    output$tab2 <- renderTable({
        chosenVal <- paste(input$house)
        top <- temp %>% 
            filter(House %in% chosenVal) %>% 
            group_by(House, Year) %>% 
            top_n(., 10, IncomeTax) %>%
            arrange(., -Year, House, -IncomeTax) %>% 
            select(Year, House, Name, IncomeTax)
        
        top %>% 
            gt()
    })
    
    # zero taxers who
    output$tab3 <- renderTable({
        chosenVal <- paste(input$house)
        zerowho <- temp %>% 
            ungroup() %>% 
            filter(House %in% chosenVal) %>% 
            filter(IncomeTax == 0) %>% 
            arrange(Year, House, IncomeTax) %>% 
            select(Year, House, Name, IncomeTax)
        
        zerowho %>% 
            gt()
    })
    
    # summary statistics
    output$tab4 <- renderTable({
        chosenVal <- paste(input$house)
        key <- temp %>%
            filter(House %in% chosenVal) %>% 
            group_by(House, Year) %>%
            summarize("Total Income Tax" = sum(IncomeTax), 
                      "Average Income Tax" = mean(IncomeTax),
                      "Median Income Tax" = median(IncomeTax),
                      "Max Income Tax" = max(IncomeTax),
                      "St. Dev Income Tax" = sd(IncomeTax))
        
        key %>% 
            gt()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)