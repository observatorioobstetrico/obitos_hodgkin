library(shiny)
library(shinydashboard)
library(tidyverse)
library(xtable)
library(knitr)
library(dplyr)
# graficos
library(ggplot2)
library(geobr)
library(scales)
library(summarytools)
library(data.table)
theme_set(theme_light())
options(knitr.kable.NA = '-')


var_utilizadas <- c(
  "regiao",
  "faixa_et",
  "ano"
)

region <- read_region(year = 2020)

# dados de hodgking 

dados_cancer <- readRDS("dados/dados_cancer.rds")

# dados populacao

dados_populacao <- readRDS("dados/dados_populacao.rds")

dados <- rbind(dados_cancer,dados_populacao)

d <- dados %>% 
  tidyr::pivot_longer(
    cols = c('2000', '2001', '2002', '2003', '2004', '2005',
             '2006', '2007', '2008', '2009', '2010', '2011',
             '2012', '2013', '2014', '2015', '2016', '2017',
             '2018', '2019', '2020'),
    names_to = "ano",
    values_to = "n"
  )

d$faixa_et <- factor(d$faixa_et,levels = c("menor_1ano","1-4","5-9","10-14","15-19"))

d <- d %>% mutate(name_region = case_when(
  regiao == "norte" ~ "Norte",
  regiao == "nordeste" ~ "Nordeste",
  regiao == "sudeste" ~ "Sudeste",
  regiao == "sul" ~ "Sul",
  regiao == "centro-oeste" ~ "Centro Oeste",
  TRUE ~ NA_character_)) 


ui <- navbarPage(
  "LINFOMA DE HODGKIN EM CRIANÇAS E ADOLESCENTES NO BRASIL",
  tabPanel(
    "Mapa geográfico",
    icon = icon("map"),
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          checkboxGroupInput(
            inputId = "faixaet",
            label = "Faixa etária",
            choices = c("Menor que 1 ano"="menor_1ano",
                        "1 a 4 anos"="1-4",
                        "5 a 9 anos"="5-9",
                        "10 a 14 anos"="10-14",
                        "15 a 19 anos"="15-19"),
            selected = c("menor_1ano","1-4","5-9","10-14","15-19")
          ),
          selectInput(
            inputId = "sel_ano",
            label = "Selecionar ano?",
            choices = c("Sim","Não"),
            selected = "Não",
          ),
          shiny::conditionalPanel(
            condition = "input.sel_ano == 'Sim'",
            selectInput(
              inputId = "anoid",
              label = "Selecione o ano",
              choices = c('2000', '2001', '2002', '2003', '2004', '2005',
                          '2006', '2007', '2008', '2009', '2010', '2011',
                          '2012', '2013', '2014', '2015', '2016', '2017',
                          '2018', '2019', '2020'),
              multiple = TRUE
            )
          )
        ),
        mainPanel(plotOutput("plot"))
      )
    )
  ),
  tabPanel(
    "Sobre",
    icon = icon("info-sign", 
                lib = "glyphicon"),
    fluidPage(
      includeMarkdown("sobre.md")
    )
  )
)

server <- function(input, output) {
  
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('sobre.md', quiet = TRUE)))
  })
  
  data <- reactive({
    d %>% 
      dplyr::filter(faixa_et %in% input$faixaet)

  })
  
  output$plot <- renderPlot({
    if(input$sel_ano == "Não"){
      t5 <- data() %>% 
        group_by(regiao,name_region,faixa_et,base) %>% 
        summarise(n_total = sum(n)) %>% 
        as.data.frame()
      
      t1 <- t5 %>% filter(base=="hodgkin" & name_region != "NA")
      
      t2 <- t5 %>% filter(base=="populacao" & name_region != "NA") %>% rename(n_pop = n_total) %>% select(-base)
      
      dados2 <- full_join(t1,t2,by=c("regiao","faixa_et","name_region")) %>% mutate(taxa=n_total/n_pop*100000)
      
      dados_mapa <- left_join(region, dados2, by= "name_region")
      
      dados_mapa %>%
        ggplot() +
        geom_sf(aes(fill=taxa), color= "grey30", size=.15) +
        theme_void()  + 
        labs(title = "Linfoma de Hodgkin, por faixa etária e região",
             subtitle = "Taxa de mortalidade a cada 100.000 habitantes",
             fill = "taxa") +
        scale_fill_viridis_c(option="rocket",direction=-1) + 
        facet_wrap(~faixa_et)
    }
    else{
      t5 <- data() %>% 
        dplyr::filter(ano %in% input$anoid) %>% 
        group_by(regiao,name_region,faixa_et,base) %>% 
        summarise(n_total = sum(n)) %>% 
        as.data.frame()
      
      t1 <- t5 %>% filter(base=="hodgkin" & name_region != "NA")
      
      t2 <- t5 %>% filter(base=="populacao" & name_region != "NA") %>% rename(n_pop = n_total) %>% select(-base)
      
      dados2 <- full_join(t1,t2,by=c("regiao","faixa_et","name_region")) %>% mutate(taxa=n_total/n_pop*100000)
      
      dados_mapa <- left_join(region, dados2, by= "name_region")
      
      dados_mapa %>%
        ggplot() +
        geom_sf(aes(fill=taxa), color= "grey30", size=.15) +
        theme_void()  + 
        labs(title = "Linfoma de Hodgkin, por faixa etária e região",
             subtitle = "Taxa de mortalidade a cada 100.000 habitantes",
             fill = "taxa") +
        scale_fill_viridis_c(option="rocket",direction=-1) + 
        facet_wrap(~faixa_et)
    }
  })
}

shinyApp(ui, server)
