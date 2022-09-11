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
                
                headerPanel("WAI Berechnungstool für WAI in der Personenberatung"),
                
                #Arbeitsart
                selectizeInput(
                  inputId = "arbeitsart",
                  label = "Sind Sie bei Ihrer Arbeit ...",
                  multiple = FALSE, 
                  selected = FALSE,
                  choices = c("",
                              "vorwiegend geistig tätig",
                              "vorwiegend körperlich tätig",
                              "etwa gleichermaßen geistig und körperlich tätig")
                  
                ),
                #Arbeitsfähigkeit psychisch und physisch
                numericInput(
                  inputId = "derzeitige_arbeitsfähigkeit",
                  label = "Wenn Sie Ihre beste, je erreichte Arbeitsfähigkeit mit 10 Punkten bewerten:\n
    Wie viele Punkte würden Sie dann für Ihre derzeitige Arbeitsfähigkeit geben?\n
    (0 bedeutet, dass Sie derzeit arbeitsunfähig sind)",
                  min = 1, max = 10, step = 1, value = FALSE
                ),
                selectizeInput(
                  inputId = "körperlich_arbeitsfähigkeit",
                  label = "Wie schätzen Sie Ihre derzeitige Arbeitsfähigkeit in Bezug auf die körperlichen Arbeitsanforderungen ein?",
                  multiple = FALSE,
                  choices = c("",
                              "sehr gut",
                              "eher gut",
                              "mittelmäßig",
                              "eher schlecht",
                              "sehr schlecht"),
                  
                ),
                selectizeInput(
                  inputId = "psychisch_arbeitsfähigkeit",
                  label = "Wie schätzen Sie Ihre derzeitige Arbeitsfähigkeit in Bezug auf die psychische Arbeitsanforderungen ein?",
                  multiple = FALSE,
                  choices = c("",
                              "sehr gut",
                              "eher gut",
                              "mittelmäßig",
                              "eher schlecht",
                              "sehr schlecht"),
                  
                ),
                ####- Unfall Krankheiten --> hier ZwischenÜberschrift einfügen
                selectizeInput(
                  inputId = "krankheit_unfall",
                  label = "Unfallverletzungen (z.B. des Rückens, der Glieder, Verbrennungen)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                  
                ),
                selectizeInput(
                  inputId = "krankheit_muskel",
                  label = "Erkrankungen des Muskel-Skelett-Systems von Rücken, Gliedern oder anderen Körperteilen\n
    (z.B. wiederholte Schmerzen in Gelenken oder Muskeln, Ischias, Rheuma, Wirbelsäulenerkrankungen)",
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
                  label = "Psychische Beeinträchtigungen (z.B. Depressionen, Angstzustände, chronische Schlaflosigkeit, psychovegetatives Erschöpfungssyndrom)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_neuro",
                  label = "Neurologische und sensorische Erkrankungen (z.B. Tinnitus, Hörschäden, Augenerkrankungen, Migräne, Epilepsie)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_verdauung",
                  label = "Erkrankungen des Verdauungssystems (z.B. der Gallenblase, Leber, Bauchspeicheldrüse, Darm)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_genital",
                  label = "Erkrankungen im Urogenitaltrakt (z.B. Harnwegsinfektionen, gynäkologische Erkrankungen)",
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
                  label = "Hormon- / Stoffwechselerkrankungen (z.B. Diabetes, Fettleibigkeit, Schilddrüsenprobleme)",
                  multiple = FALSE,
                  choices = c("",
                              "eigene Diagnose",
                              "Diagnose vom Arzt",
                              "liegt nicht vor"),
                ),
                selectizeInput(
                  inputId = "krankheit_blut",
                  label = "Krankheiten des Blutes (z.B. Anämie)",
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
                
                ## Geschätzte Beeinträchtigung der Arbeitsleistung durch die Krankheiten 
                selectizeInput(
                  inputId = "beeinträchtigung_1",
                  label = "Keine Beeinträchtigung / Ich habe keine Erkrankung",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  ),
                ),
                selectizeInput(
                  inputId = "beeinträchtigung_2",
                  label = "Ich kann meine Arbeit ausführen, habe aber Beschwerden",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                selectizeInput(
                  inputId = "beeinträchtigung_3",
                  label = "Ich bin manchmal gezwungen, langsamer zu arbeiten\n
    oder meine Arbeitsmethoden zu ändern",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                selectizeInput(
                  inputId = "beeinträchtigung_4",
                  label = "Ich bin oft gezwungen, langsamer zu arbeiten\n
    oder meine Arbeitsmethoden zu ändern",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                selectizeInput(
                  inputId = "beeinträchtigung_5",
                  label = "Wegen meiner Krankheit bin ich nur\n
    in der Lage Teilzeitarbeit zu verrichten",
                  multiple = FALSE,
                  choices = c("",
                              "zutreffend",
                              "nicht zutreffend"
                  )
                ),
                selectizeInput(
                  inputId = "beeinträchtigung_6",
                  label = "Meiner Meinung nach bin ich völlig arbeitsunfähig",
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
                              "überhaupt keinen",
                              "höchstens 9 Tage",
                              "10-24 Tage",
                              "25-99 Tage",
                              "100-365 Tage"
                              
                  )
                ),
                selectizeInput(
                  inputId = "arbeitsfähigkeit_zukunft",
                  label = "Glauben Sie, dass Sie, ausgehend von Ihrem jetzigen Gesundheitszustand,\n
    Ihre derzeitige Arbeit auch in den nächsten zwei Jahren ausüben können?",
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
                  label = "Haben Sie in der letzten Zeit Ihre täglichen Aufgaben mit Freude erledigt?",
                  multiple = FALSE,
                  choices = c("",
                              "häufig",
                              "eher häufig",
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
                              "häufig",
                              "eher häufig",
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
                              "häufig",
                              "eher häufig",
                              "manchmal",
                              "eher selten",
                              "niemals"
                              
                  )
                ),
                #Krankenstand im vergangenen Jahr (12 Monate)
                selectizeInput(
                  inputId = "krankenstand_präzise",
                  label = "Sie haben bei Frage 6 bereits Angaben zu Krankenstands-Tagen\n
    (aufgrund von Krankheit, Gesundheitsvorsorge, Untersuchung) im vergangenen Jahr\n
    (12 Monate) gemacht. Bitte spezifizieren Sie Ihre Aussage:\n
    In welchem Ausmaß haben Sie in den vergangenen 12 Monaten Krankenstand\n
    in Anspruch genommen?",
                  multiple = FALSE,
                  choices = c("",
                              "überhaupt keinen",
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
  
  #Funktionen für WAI Pipe
  leistungsreserven_fun <- function(x) {
    case_when(x %in% c("häufig", "immer", "ständig") ~ 4,
              x %in% c("eher häufig") ~ 3,
              x %in% c("manchmal") ~ 2,
              x %in% c("eher selten") ~ 1,
              x %in% c("niemals") ~ 0)
  }
  krankheit_fun <- function(x) {
    case_when(x %in% c("eigene Diagnose") ~ 2,
              x %in% c("Diagnose vom Arzt") ~ 1,
              x %in% c("liegt nicht vor") ~ 0)
  }
  arbeitsfähigkeit_fun <- function(x){
    case_when(x %in% c("sehr gut") ~ 5,
              x %in% c("eher gut") ~ 4,
              x %in% c("mittelmäßig") ~ 3,
              x %in% c("eher schlecht") ~ 2,
              x %in% c("sehr schlecht") ~ 1)
  }
  
  # Umrechnungs Pipe (Text zu Zahl und Zahl zu WAI Berechnung)
  data_raw <- reactive({
    df <- data.frame(arbeitsart = input$arbeitsart, #----
                     derzeitige_arbeitsfähigkeit = input$derzeitige_arbeitsfähigkeit,
                     körperlich_arbeitsfähigkeit = input$körperlich_arbeitsfähigkeit,
                     psychisch_arbeitsfähigkeit = input$psychisch_arbeitsfähigkeit,
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
                     beeinträchtigung_1 = input$beeinträchtigung_1,
                     beeinträchtigung_2 = input$beeinträchtigung_2,
                     beeinträchtigung_3 = input$beeinträchtigung_3,
                     beeinträchtigung_4 = input$beeinträchtigung_4,
                     beeinträchtigung_5 = input$beeinträchtigung_5,
                     beeinträchtigung_6 = input$beeinträchtigung_6,
                     krankenstand = input$krankenstand,
                     arbeitsfähigkeit_zukunft = input$arbeitsfähigkeit_zukunft,
                     leistungsreserven_1 = input$leistungsreserven_1,
                     leistungsreserven_2 = input$leistungsreserven_2,
                     leistungsreserven_3 = input$leistungsreserven_3,
                     krankenstand_präzise = input$krankenstand_präzise) %>% #31 Inputs
      mutate(arbeitsart = case_when(arbeitsart == "vorwiegend geistig tätig" ~ 1,
                                    arbeitsart == "vorwiegend körperlich tätig" ~ 2,
                                    arbeitsart == "etwa gleichermaßen geistig und körperlich tätig" ~ 3)) %>% 
      mutate(derzeitige_arbeitsfähigkeit = case_when(derzeitige_arbeitsfähigkeit == "0" ~ 0,
                                                     derzeitige_arbeitsfähigkeit == "1" ~ 1,
                                                     derzeitige_arbeitsfähigkeit == "2" ~ 2,
                                                     derzeitige_arbeitsfähigkeit == "3" ~ 3,
                                                     derzeitige_arbeitsfähigkeit == "4" ~ 4,
                                                     derzeitige_arbeitsfähigkeit == "5" ~ 5,
                                                     derzeitige_arbeitsfähigkeit == "6" ~ 6,
                                                     derzeitige_arbeitsfähigkeit == "7" ~ 7,
                                                     derzeitige_arbeitsfähigkeit == "8" ~ 8,
                                                     derzeitige_arbeitsfähigkeit == "9" ~ 9,
                                                     derzeitige_arbeitsfähigkeit == "10" ~ 10)) %>% 
      mutate_at(c(3:4), arbeitsfähigkeit_fun) %>% #psychische und körperliche Arbeitsfähigkeit
      mutate_at(c(5:18), krankheit_fun)  %>% #Alle Krankheiten
      mutate(beeinträchtigung_1 = case_when(beeinträchtigung_1 == "zutreffend" ~ 1,
                                            beeinträchtigung_1 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeinträchtigung_2 = case_when(beeinträchtigung_2 == "zutreffend" ~ 2,
                                            beeinträchtigung_2 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeinträchtigung_3 = case_when(beeinträchtigung_3 == "zutreffend" ~ 3,
                                            beeinträchtigung_3 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeinträchtigung_4 = case_when(beeinträchtigung_4 == "zutreffend" ~ 4,
                                            beeinträchtigung_4 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeinträchtigung_5 = case_when(beeinträchtigung_5 == "zutreffend" ~ 5,
                                            beeinträchtigung_5 == "nicht zutreffend" ~ 0)) %>% 
      mutate(beeinträchtigung_6 = case_when(beeinträchtigung_6 == "zutreffend" ~ 6,
                                            beeinträchtigung_6 == "nicht zutreffend" ~ 0)) %>% 
      mutate(krankenstand = case_when(krankenstand == "überhaupt keinen" ~ 1,
                                      krankenstand == "höchstens 9 Tage" ~ 2,
                                      krankenstand == "10-24 Tage" ~ 3,
                                      krankenstand == "25-99 Tage" ~ 4,
                                      krankenstand == "100-365 Tage" ~ 5)) %>%
      mutate(arbeitsfähigkeit_zukunft = case_when(arbeitsfähigkeit_zukunft == "ziemlich sicher" ~ 7,
                                                  arbeitsfähigkeit_zukunft == "nicht sicher" ~ 4,
                                                  arbeitsfähigkeit_zukunft == "unwahrscheinlich" ~ 1)) %>% 
      mutate_at(c(28:30), leistungsreserven_fun) %>% #alle Leistungsreserven
      mutate(krankenstand_präzise = case_when(krankenstand_präzise == "überhaupt keinen" ~ 1,
                                              krankenstand_präzise == "1-3 Tag(e)" ~ 2,
                                              krankenstand_präzise == "4-6 Tage" ~ 3,
                                              krankenstand_präzise == "7-9 Tage" ~ 4,
                                              krankenstand_präzise == "mehr als 9 Tage" ~ 5)) %>% 
      mutate_at(c(1:18, 20:31), as.numeric) %>% #WAI Berechnung ab hier
      mutate(Dimension_1 = derzeitige_arbeitsfähigkeit) %>% 
      mutate(Dimension_2 = case_when(arbeitsart == 1 ~ (körperlich_arbeitsfähigkeit * 0.5) + (psychisch_arbeitsfähigkeit * 1.5),
                               arbeitsart == 2 ~ (körperlich_arbeitsfähigkeit * 1.5) + (psychisch_arbeitsfähigkeit * 0.5),
                               arbeitsart == 3 ~ körperlich_arbeitsfähigkeit + psychisch_arbeitsfähigkeit)) %>% 
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
      mutate(Dimension_6 = arbeitsfähigkeit_zukunft) %>% 
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



