library(tidyverse)
library(shiny)
#library(shiny.fluent) <-- corrupted file...??






##Shiny App--------
# shinyUI(navbarPage("My Application",
#                    tabPanel("Component 1"), 
#                    tabPanel("Component 2"),
#                    tabPanel("Component 3")
# ))


ui <- fluidPage(####----
                
                headerPanel("WAI Berechnungstool fuer WAI in der Personenberatung"),
                
                #Arbeitsart
                selectizeInput(
                  inputId = "arbeitsart",
                  label = "Sind Sie bei Ihrer Arbeit ...",
                  multiple = FALSE, 
                  selected = FALSE,
                  choices = c("",
                              "vorwiegend geistig taetig",
                              "vorwiegend koerperlich taetig",
                              "etwa gleichermassen geistig und koerperlich taetig")
                  
                ),
                #Arbeitsfaehigkeit psychisch und physisch
                numericInput(
                  inputId = "derzeitige_arbeitsfaehigkeit",
                  label = "Wenn Sie Ihre beste, je erreichte Arbeitsfaehigkeit mit 10 Punkten bewerten:\n
    Wie viele Punkte wuerden Sie dann fuer Ihre derzeitige Arbeitsfaehigkeit geben?\n
    (0 bedeutet, dass Sie derzeit arbeitsunfaehig sind)",
                  min = 1, max = 10, step = 1, value = FALSE
                ),
                selectizeInput(
                  inputId = "koerperlich_arbeitsfaehigkeit",
                  label = "Wie schaetzen Sie Ihre derzeitige Arbeitsfaehigkeit in Bezug auf die koerperlichen Arbeitsanforderungen ein?",
                  multiple = FALSE,
                  choices = c("",
                              "sehr gut",
                              "eher gut",
                              "mittelmaessig",
                              "eher schlecht",
                              "sehr schlecht"),
                  
                ),
                selectizeInput(
                  inputId = "psychisch_arbeitsfaehigkeit",
                  label = "Wie schaetzen Sie Ihre derzeitige Arbeitsfaehigkeit in Bezug auf die psychische Arbeitsanforderungen ein?",
                  multiple = FALSE,
                  choices = c("",
                              "sehr gut",
                              "eher gut",
                              "mittelmaessig",
                              "eher schlecht",
                              "sehr schlecht"),
                  
                ),
                ####- Unfall Krankheiten --> hier Zwischenueberschrift einfuegen
                selectizeInput(
                  inputId = "krankheit_unfall",
                  label = "Unfallverletzungen (z.B. des Rueckens, der Glieder, Verbrennungen)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                  
                ),
                selectizeInput(
                  inputId = "krankheit_muskel",
                  label = "Erkrankungen des Muskel-Skelett-Systems von Ruecken, Gliedern oder anderen Koerperteilen\n
    (z.B. wiederholte Schmerzen in Gelenken oder Muskeln, Ischias, Rheuma, Wirbelsaeulenerkrankungen)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                  
                ),
                selectizeInput(
                  inputId = "krankheit_herz",
                  label = "Herz-Kreislauf-Erkrankungen(z.B. Bluthochdruck, Herzkrankzeit, Herzinfarkt)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                  
                ),
                selectizeInput(
                  inputId = "krankheit_atem",
                  label = "Atemwegserkrankungen(z.B. wiederholte Atemwegsinfektionen, chronische Bronchitis, Bronchialasthma)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                  
                ),
                selectizeInput(
                  inputId = "krankheit_mental",
                  label = "Psychische Beeintraechtigungen (z.B. Depressionen, Angstzustaende, chronische Schlaflosigkeit, psychovegetatives Erschoepfungssyndrom)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_neuro",
                  label = "Neurologische und sensorische Erkrankungen (z.B. Tinnitus, Hoerschaeden, Augenerkrankungen, Migraene, Epilepsie)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_verdauung",
                  label = "Erkrankungen des Verdauungssystems (z.B. der Gallenblase, Leber, Bauchspeicheldruese, Darm)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_genital",
                  label = "Erkrankungen im Urogenitaltrakt (z.B. Harnwegsinfektionen, gynaekologische Erkrankungen)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_haut",
                  label = "Hautkrankheiten (z.B. allergischer Hautausschlag, Ekzem)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_tumor",
                  label = "Tumore / Krebs",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_hormon",
                  label = "Hormon- / Stoffwechselerkrankungen (z.B. Diabetes, Fettleibigkeit, Schilddruesenprobleme)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_blut",
                  label = "Krankheiten des Blutes (z.B. Anaemie)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_angeboren",
                  label = "Angeborene Leiden / Erkrankungen",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_andere",
                  label = "Andere Leiden oder Krankheiten",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                textInput(
                  inputId = "krankheit_andere2",
                  label = "Andere Krankheiten - bitte eintragen, falls vorhanden: ",
                ),
                
                ## Geschaetzte Beeintraechtigung der Arbeitsleistung durch die Krankheiten 
                selectizeInput(
                  inputId = "beeintraechtigung_1",
                  label = "Keine Beeintraechtigung / Ich habe keine Erkrankung",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  ),
                ),
                selectizeInput(
                  inputId = "beeintraechtigung_2",
                  label = "Ich kann meine Arbeit ausfuehren, habe aber Beschwerden",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                selectizeInput(
                  inputId = "beeintraechtigung_3",
                  label = "Ich bin manchmal gezwungen, langsamer zu arbeiten\n
    oder meine Arbeitsmethoden zu aendern",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                selectizeInput(
                  inputId = "beeintraechtigung_4",
                  label = "Ich bin oft gezwungen, langsamer zu arbeiten\n
    oder meine Arbeitsmethoden zu aendern",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                selectizeInput(
                  inputId = "beeintraechtigung_5",
                  label = "Wegen meiner Krankheit bin ich nur\n
    in der Lage Teilzeitarbeit zu verrichten",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                selectizeInput(
                  inputId = "beeintraechtigung_6",
                  label = "Meiner Meinung nach bin ich voellig arbeitsunfaehig",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                ##Krankenstand im vergangenen Jahr (12 Monate)
                selectizeInput(
                  inputId = "krankenstand",
                  label = "Wie viele ganze Tage blieben Sie auf Grund eines gesundheitlichen\n
    Problems (Krankheit, Gesundheitsvorsorge oder Untersuchung)\n
    im letzten Jahr (12 Monate) der Arbeit fern?",
                  multiple = FALSE,
                  choices = c("",
                              "ueberhaupt keinen",
                              "hoechstens 9 Tage",
                              "10-24 Tage",
                              "25-99 Tage",
                              "100-365 Tage"
                              
                  )
                ),
                selectizeInput(
                  inputId = "arbeitsfaehigkeit_zukunft",
                  label = "Glauben Sie, dass Sie, ausgehend von Ihrem jetzigen Gesundheitszustand,\n
    Ihre derzeitige Arbeit auch in den naechsten zwei Jahren ausueben koennen?",
                  multiple = FALSE,
                  choices = c("",
                              "unwahrscheinlich",
                              "nicht sicher",
                              "ziemlich sicher"
                              
                  )
                ),
                
                #psychische Leistungsreservern
                selectizeInput(
                  inputId = "leistungsreserven_1",
                  label = "Haben Sie in der letzten Zeit Ihre taeglichen Aufgaben mit Freude erledigt?",
                  multiple = FALSE,
                  choices = c("",
                              "haeufig",
                              "eher haeufig",
                              "manchmal",
                              "eher selten",
                              "niemals"
                              
                  )
                ),
                selectizeInput(
                  inputId = "leistungsreserven_2",
                  label = "Waren Sie in letzter Zeit aktiv und rege?",
                  multiple = FALSE,
                  choices = c("",
                              "haeufig",
                              "eher haeufig",
                              "manchmal",
                              "eher selten",
                              "niemals"
                              
                  )
                ),
                selectizeInput(
                  inputId = "leistungsreserven_3",
                  label = "Waren Sie in letzter Zeit zuversichtlich, was die Zukunft betrifft?",
                  multiple = FALSE,
                  choices = c("",
                              "haeufig",
                              "eher haeufig",
                              "manchmal",
                              "eher selten",
                              "niemals"
                              
                  )
                ),
                #Krankenstand im vergangenen Jahr (12 Monate)
                selectizeInput(
                  inputId = "krankenstand_praezise",
                  label = "Sie haben bei Frage 6 bereits Angaben zu Krankenstands-Tagen\n
    (aufgrund von Krankheit, Gesundheitsvorsorge, Untersuchung) im vergangenen Jahr\n
    (12 Monate) gemacht. Bitte spezifizieren Sie Ihre Aussage:\n
    In welchem Ausmass haben Sie in den vergangenen 12 Monaten Krankenstand\n
    in Anspruch genommen?",
                  multiple = FALSE,
                  choices = c("",
                              "ueberhaupt keinen",
                              "1-3 Tage",
                              "4-6 Tage",
                              "7-9 Tage",
                              "mehr als 9 Tage"
                              
                  )
                ),
                
                #WAI Berechnen Button
                actionButton(
                  inputId = "WAI_button",
                  label = "WAI Berechnen"
                ),
                # 
                # #Output Fenster
                # verbatimTextOutput(
                #   outputId = "WAI_gesamt",
                #   placeholder = TRUE
                # ), 
                
                dataTableOutput(
                  outputId = "table"
                )
)


server <- function(input, output, session) {
  
  #Funktionen fuer WAI Pipe
  leistungsreserven_fun <- function(x) {
    case_when(x %in% c("haeufig", "immer", "staendig") ~ 4,
              x %in% c("eher haeufig") ~ 3,
              x %in% c("manchmal") ~ 2,
              x %in% c("eher selten") ~ 1,
              x %in% c("niemals") ~ 0)
  }
  krankheit_fun <- function(x) {
    case_when(x %in% c("eigene Diagnose") ~ 2,
              x %in% c("Diagnose vom Arzt") ~ 1,
              x %in% c("liegt nicht vor") ~ 0)
  }
  arbeitsfaehigkeit_fun <- function(x){
    case_when(x %in% c("sehr gut") ~ 5,
              x %in% c("eher gut") ~ 4,
              x %in% c("mittelmaessig") ~ 3,
              x %in% c("eher schlecht") ~ 2,
              x %in% c("sehr schlecht") ~ 1)
  }
  
  # Umrechnungs Pipe (Text zu Zahl und Zahl zu WAI Berechnung)
  data_raw <- reactive({
    df <- data.frame(arbeitsart = input$arbeitsart, #----
                     derzeitige_arbeitsfaehigkeit = input$derzeitige_arbeitsfaehigkeit,
                     koerperlich_arbeitsfaehigkeit = input$koerperlich_arbeitsfaehigkeit,
                     psychisch_arbeitsfaehigkeit = input$psychisch_arbeitsfaehigkeit,
                     krankheit_unfall = input$krankheit_unfall, #5
                     krankheit_muskel = input$krankheit_muskel,
                     krankheit_herz = input$krankheit_herz,
                     krankheit_atem = input$krankheit_atem,
                     krankheit_mental = input$krankheit_mental,
                     krankheit_neuro = input$krankheit_neuro,
                     krankheit_verdauung = input$krankheit_verdauung,
                     krankheit_genital = input$krankheit_genital,
                     krankheit_haut = input$krankheit_haut,
                     krankheit_tumor = input$krankheit_tumor,
                     krankheit_hormon = input$krankheit_hormon,
                     krankheit_blut = input$krankheit_blut,
                     krankheit_angeboren = input$krankheit_angeboren,
                     krankheit_andere = input$krankheit_andere, #18
                     krankheit_andere2 = input$krankheit_andere2,
                     beeintraechtigung_1 = input$beeintraechtigung_1,
                     beeintraechtigung_2 = input$beeintraechtigung_2,
                     beeintraechtigung_3 = input$beeintraechtigung_3,
                     beeintraechtigung_4 = input$beeintraechtigung_4,
                     beeintraechtigung_5 = input$beeintraechtigung_5,
                     beeintraechtigung_6 = input$beeintraechtigung_6,
                     krankenstand = input$krankenstand,
                     arbeitsfaehigkeit_zukunft = input$arbeitsfaehigkeit_zukunft,
                     leistungsreserven_1 = input$leistungsreserven_1,
                     leistungsreserven_2 = input$leistungsreserven_2,
                     leistungsreserven_3 = input$leistungsreserven_3,
                     krankenstand_praezise = input$krankenstand_praezise) %>% #31 Inputs
      mutate(arbeitsart = case_when(arbeitsart == "vorwiegend geistig taetig" ~ 1,
                                    arbeitsart == "vorwiegend koerperlich taetig" ~ 2,
                                    arbeitsart == "etwa gleichermassen geistig und koerperlich taetig" ~ 3)) %>% 
      mutate(derzeitige_arbeitsfaehigkeit = case_when(derzeitige_arbeitsfaehigkeit == "0" ~ 0,
                                                     derzeitige_arbeitsfaehigkeit == "1" ~ 1,
                                                     derzeitige_arbeitsfaehigkeit == "2" ~ 2,
                                                     derzeitige_arbeitsfaehigkeit == "3" ~ 3,
                                                     derzeitige_arbeitsfaehigkeit == "4" ~ 4,
                                                     derzeitige_arbeitsfaehigkeit == "5" ~ 5,
                                                     derzeitige_arbeitsfaehigkeit == "6" ~ 6,
                                                     derzeitige_arbeitsfaehigkeit == "7" ~ 7,
                                                     derzeitige_arbeitsfaehigkeit == "8" ~ 8,
                                                     derzeitige_arbeitsfaehigkeit == "9" ~ 9,
                                                     derzeitige_arbeitsfaehigkeit == "10" ~ 10)) %>% 
      mutate_at(c(3:4), arbeitsfaehigkeit_fun) %>% #psychische und koerperliche Arbeitsfaehigkeit
      mutate_at(c(5:18), krankheit_fun)  %>% #Alle Krankheiten
      mutate(beeintraechtigung_1 = case_when(beeintraechtigung_1 == "zutreffend" ~ 1,
                                            beeintraechtigung_1 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeintraechtigung_2 = case_when(beeintraechtigung_2 == "zutreffend" ~ 2,
                                            beeintraechtigung_2 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeintraechtigung_3 = case_when(beeintraechtigung_3 == "zutreffend" ~ 3,
                                            beeintraechtigung_3 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeintraechtigung_4 = case_when(beeintraechtigung_4 == "zutreffend" ~ 4,
                                            beeintraechtigung_4 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeintraechtigung_5 = case_when(beeintraechtigung_5 == "zutreffend" ~ 5,
                                            beeintraechtigung_5 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeintraechtigung_6 = case_when(beeintraechtigung_6 == "zutreffend" ~ 6,
                                            beeintraechtigung_6 == "nicht zutreffend" ~ 0)) %>% 
      mutate(krankenstand = case_when(krankenstand == "ueberhaupt keinen" ~ 1,
                                      krankenstand == "hoechstens 9 Tage" ~ 2,
                                      krankenstand == "10-24 Tage" ~ 3,
                                      krankenstand == "25-99 Tage" ~ 4,
                                      krankenstand == "100-365 Tage" ~ 5)) %>%
      mutate(arbeitsfaehigkeit_zukunft = case_when(arbeitsfaehigkeit_zukunft == "ziemlich sicher" ~ 7,
                                                  arbeitsfaehigkeit_zukunft == "nicht sicher" ~ 4,
                                                  arbeitsfaehigkeit_zukunft == "unwahrscheinlich" ~ 1)) %>% 
      mutate_at(c(28:30), leistungsreserven_fun) %>% #alle Leistungsreserven
      mutate(krankenstand_praezise = case_when(krankenstand_praezise == "ueberhaupt keinen" ~ 1,
                                              krankenstand_praezise == "1-3 Tag(e)" ~ 2,
                                              krankenstand_praezise == "4-6 Tage" ~ 3,
                                              krankenstand_praezise == "7-9 Tage" ~ 4,
                                              krankenstand_praezise == "mehr als 9 Tage" ~ 5)) %>% 
      mutate_at(c(1:18, 20:31), as.numeric) %>% #WAI Berechnung ab hier
      mutate(Dimension_1 = derzeitige_arbeitsfaehigkeit) %>% 
      mutate(Dimension_2 = case_when(arbeitsart == 1 ~ (koerperlich_arbeitsfaehigkeit * 0.5) + (psychisch_arbeitsfaehigkeit * 1.5),
                               arbeitsart == 2 ~ (koerperlich_arbeitsfaehigkeit * 1.5) + (psychisch_arbeitsfaehigkeit * 0.5),
                               arbeitsart == 3 ~ koerperlich_arbeitsfaehigkeit + psychisch_arbeitsfaehigkeit)) %>% 
      mutate(Dimension_3 = case_when(rowSums(across(5:18) == 1) == 0 ~ 7,
                               rowSums(across(5:18) == 1) == 1 ~ 5,
                               rowSums(across(5:18) == 1) == 2 ~ 3,
                               rowSums(across(5:18) == 1) == 3 ~ 3,
                               rowSums(across(5:18) == 1) >= 4 ~ 1)) %>% 
      mutate_at(vars(starts_with("beein")), ~replace_na(., 0)) %>% 
      rowwise() %>% 
      mutate(Dimension_4 = case_when(max(across(20:25)) == 1 ~ 6,
                               max(across(20:25)) == 2 ~ 5,
                               max(across(20:25)) == 3 ~ 4,
                               max(across(20:25)) == 4 ~ 3,
                               max(across(20:25)) == 5 ~ 2,
                               max(across(20:25)) == 6 ~ 1)) %>% 
      ungroup() %>% 
      mutate(Dimension_5 = case_when(krankenstand == 1 ~ 5,
                               krankenstand == 2 ~ 4,
                               krankenstand == 3 ~ 3,
                               krankenstand == 4 ~ 2,
                               krankenstand == 5 ~ 1)) %>% 
      mutate(Dimension_6 = arbeitsfaehigkeit_zukunft) %>% 
      rowwise() %>% 
      mutate(Dimension_7 = case_when(rowSums(across(28:30)) <= 3 ~ 1,
                               rowSums(across(28:30)) == 4 ~ 2,
                               rowSums(across(28:30)) == 5 ~ 2,
                               rowSums(across(28:30)) == 6 ~ 2,
                               rowSums(across(28:30)) == 7 ~ 3,
                               rowSums(across(28:30)) == 8 ~ 3,
                               rowSums(across(28:30)) == 9 ~ 3,
                               rowSums(across(28:30)) >= 10 ~ 4)) %>%
      mutate(WAI = rowSums(across(32:38))) %>% 
      select(Dimension_1, Dimension_2, Dimension_3, Dimension_4, Dimension_5, Dimension_6, Dimension_7, WAI)
  }) %>% 
    bindEvent(input$WAI_button)
  
  
  output$table <- renderDataTable({
    data_raw()
  })
}



shinyApp(ui, server)



