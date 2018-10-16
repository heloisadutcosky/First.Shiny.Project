# server

shinyServer(function(input, output, session){
  # mostrar mapa do Brasil
  
  observe(
    updateSelectizeInput(session, "deputado",
                         choices = unique(reembolsos$deputy_name[reembolsos$political_party == input$partido])[order(unique(reembolsos$deputy_name[reembolsos$political_party == input$partido]))],
                         selected = unique(reembolsos$deputy_name[reembolsos$political_party == input$partido])[order(unique(reembolsos$deputy_name[reembolsos$political_party == input$partido]))][1]
    ))
  
  output$totalBox = renderInfoBox({
    total_value <- round(sum(gastos_por_deputado$deputados_gpa[gastos_por_deputado$year == input$year_map]) / 10 ^ 6, 2)
    infoBox(title = "Total spent", value = paste(total_value, "M R$"), subtitle = paste("spended with reimbursements in", input$year_map), icon = icon("hand-o-up"), 
            color = "light-blue", fill = T)
  })
  
  output$deputyBox = renderInfoBox({
    n_deputies <- length(unique(gastos_por_deputado$deputy_name[gastos_por_deputado$year == input$year_map]))
    infoBox(title = "Deputies", value = n_deputies, subtitle = paste("deputies in", input$year_map), icon = icon("user"),
            color = "blue", fill = T)
  })
  
  output$avgBox = renderInfoBox({
    avg_value <- round(sum(gastos_por_deputado$deputados_gpa[gastos_por_deputado$year == input$year_map]) / 
                         length(unique(gastos_por_deputado$deputy_name[gastos_por_deputado$year == input$year_map])) / 10 ^ 3, 2)
    infoBox(title = "Average spent", value = paste(avg_value, "m R$"), subtitle = paste("of reimbursements per deputy in", input$year_map), icon = icon("money"),
            color = "navy", fill = T)
  })
  
  output$stateBox = renderInfoBox({
    max_value <- max(estados$estado_mgpd[estados$year == input$year_map])
    max_state = estados$state_code[estados$estado_mgpd == max_value & estados$year == input$year_map]
    
    infoBox(title = paste("Expenditure champion of", input$year_map), 
            value = paste(max_state), 
            subtitle = paste(round(max_value/10^3, 2), "m R$ of reimbursements per deputy"), icon = icon("trophy"),
            color = "black", fill = T)
  })
  
  output$partyavgBox = renderInfoBox({
    avg_party_value = max(partidos$partido_mgpd[partidos$year == input$year_party])
    avg_party = partidos$political_party[partidos$partido_mgpd == avg_party_value & 
                                           partidos$year == input$year_party]
    
    infoBox(title = paste("Biggest average spender of", input$year_party), 
           value = paste(avg_party), 
           subtitle = paste(round(avg_party_value / 10^3, 2), "m R$ of reimbursements per deputy"), icon = icon("trophy"),
           color = "maroon", fill = T)
  })
  
  
  output$depBox = renderInfoBox({
    max_dep_value = max(gastos_por_deputado$deputados_gpa[gastos_por_deputado$year == input$year_party])
    max_dep = gastos_por_deputado$deputy_name[gastos_por_deputado$deputados_gpa == max_dep_value & 
                                                     gastos_por_deputado$year == input$year_party]
    max_dep_party = gastos_por_deputado$political_party[gastos_por_deputado$deputados_gpa == max_dep_value & 
                                                gastos_por_deputado$year == input$year_party]
  
    infoBox(title = paste("Biggest spender of", input$year_party), 
            value = paste(max_dep_party, "-", max_dep), 
            subtitle = paste(round(max_dep_value / 10^3, 2), "m R$ of reimbursements"), icon = icon("trophy"),
            color = "red", fill = T)
  })
  
  output$partymaxBox = renderInfoBox({
    per_party_state = group_by(reembolsos, year, state_code, political_party) %>%
      summarise(avg_party_state = sum(value_receipts) / n_distinct(deputy_name))
    
    max_party_state_value = max(per_party_state$avg_party_state[per_party_state$year == input$year_party])
    max_party_state = per_party_state$political_party[per_party_state$avg_party_state == max_party_state_value & 
                                           per_party_state$year == input$year_party]
    max_state_party = per_party_state$state_code[per_party_state$avg_party_state == max_party_state_value & 
                                                   per_party_state$year == input$year_party]
    
    infoBox(title = paste("Biggest average spender of", input$year_party), 
            value = paste(max_party_state, "-", max_state_party), 
            subtitle = paste(round(max_party_state_value / 10^3, 2), "m R$ of reimbursements per deputy"), icon = icon("trophy"),
            color = "orange", fill = T)
  })
  
  output$avgexpBox = renderInfoBox({
    
    avgexp = mean(gastos_por_deputado$deputados_gpa[gastos_por_deputado$deputy_name == input$deputado])
    infoBox(title = paste("Average expenditure of", input$deputado), value = paste(round(avgexp / 10^3, 2), "m R$"), subtitle = "per year",
            color = "green", fill = T)
  })
  
  output$maxexpBox = renderInfoBox({
    
    maxexp = max(gastos_por_deputado$deputados_gpa[gastos_por_deputado$deputy_name == input$deputado])
    yearexp = gastos_por_deputado$year[gastos_por_deputado$deputy_name == input$deputado & gastos_por_deputado$deputados_gpa == maxexp]
    
    infoBox(title = paste("Maximum expenditure of", input$deputado), value = paste(round(maxexp / 10^3, 2), "m R$"), 
            subtitle = paste("in",yearexp),
            color = "olive", fill = T)
  })
  
  output$maxcatBox = renderInfoBox({
    category = group_by(reembolsos, receipt_description, deputy_name) %>%
      summarise(avg_category = mean(value_receipts))
    
    maxcat_value = max(category$avg_category[category$deputy_name == input$deputado])
    maxcat = category$receipt_description[category$avg_category == maxcat_value & category$deputy_name == input$deputado]
    
    infoBox(title = paste(input$deputado, "spends more in"), value = paste(maxcat), 
            subtitle = paste("around", round(maxcat_value / 10^3, 2), "m R$ per year"),
            color = "teal", fill = T)
    
  })
  
  output$maxreceiptBox = renderInfoBox({
    maxi_receipt = max(max_receipt$`Top Receipt [R$]`)
    dep_receipt = max_receipt$Name[max_receipt$`Top Receipt [R$]` == maxi_receipt]
    cat_receipt = max_receipt$Category[max_receipt$`Top Receipt [R$]` == maxi_receipt]
    year_receipt = max_receipt$Year[max_receipt$`Top Receipt [R$]` == maxi_receipt]
    infoBox(title = paste("Biggest receipt ever:"), 
            value = paste(round(maxi_receipt / 10^3,2), "m R$"), 
            subtitle = paste("spent in", cat_receipt, "in", year_receipt, "by", dep_receipt),
            color = "black", fill = T)
  })
  
  output$maxreceiptcategoryBox = renderInfoBox({
    max_receiptcategory = max(max_receipt$`Top Receipt [R$]`[max_receipt$Category == input$category])
    dep_receiptcategory = max_receipt$Name[max_receipt$`Top Receipt [R$]` == max_receiptcategory &
                                           max_receipt$Category == input$category]
    year_receiptcategory = max_receipt$Year[max_receipt$`Top Receipt [R$]` == max_receiptcategory &
                                             max_receipt$Category == input$category]
    infoBox(title = paste("Biggest receipt within", input$category, ":"), 
            value = paste(round(max_receiptcategory / 10^3,2), "m R$"), 
            subtitle = paste("from", dep_receiptcategory, "in", year_receiptcategory),
            color = "navy", fill = T)
  })
  
  
  output$map <- renderGvis({
    filter(estados, year == input$year_map) %>%
    gvisGeoChart(., "codUF", "estado_mgpd",
                 options=list(region="BR", displayMode="regions",
                              resolution="provinces",
                              width=550, height=350, left = "auto", top = 70,
                              enableRegionInteractivity = T, 
                              regionClick = T))
    })
  
  
  output$estado <- renderPlotly({
    group_by(reembolsos, year, state_code, political_party, deputy_name) %>%
      summarise(., total_deputado = sum(value_receipts)) %>%
      filter(., state_code == input$state, year == input$year_map) %>%
      ggplot(., aes(x = political_party, y = total_deputado / 10^3)) + geom_boxplot() +
      geom_point(aes(color = deputy_name)) + 
      theme(legend.position = "none") +
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Pastel1"))(20)) + ylab("Expenditure (BRL)") +
      xlab("Political party")
  })
  
  
  
  # mostrar gráfico de bolhas
  output$bolhas <- renderGvis({
    filter(partidos, year == input$year_party) %>%
      gvisBubbleChart(., idvar = "political_party", xvar = "partido_maxgpd", yvar = "partido_mgpd",
                      colorvar = "political_party", sizevar = "n_deputados",
                      options = list(width = 900, height = 450, 
                                     titleTextStyle = "{color: 'gray', fontSize: 20}",
                                      series =  "{'AVANTE': {color: '#F2F3F4'}, 'DEM': {color: '#AED6F1'}, 'PCDOB': {color: '#F5B7B1'}, 
                                                'PDT': {color: '#3498DB'}, 'PEN': {color: '#F2F3F4'}, 'PHS': {color: '#F2F3F4'}, 
                                                'PMDB': {color: '#F1C40F'}, 'PMN': {color: '#F2F3F4'}, 'PODE': {color: '#F2F3F4'},
                                                'PP': {color: '#5499C7'}, 'PPS': {color: '#F2F3F4'}, 'PR': {color: '#7FB3D5'}, 
                                                'PRB': {color: '#F2F3F4'}, 'PROS': {color: '#F2F3F4'}, 'PRTB': {color: '#F2F3F4'}, 
                                                'PSB': {color: '#F9E79F'}, 'PSC': {color: '#7DCEA0'}, 'PSD': {color: '#F2F3F4'}, 
                                                'PSDB': {color: '#2E86C1'}, 'PSL': {color: '#34495E'}, 'PSOL': {color: '#F2F3F4'}, 
                                                'PT': {color: '#E74C3C'}, 'PTB': {color: '#1C2833'}, 'PTC': {color: '#F2F3F4'}, 
                                                'PV': {color: '#196F3D'}, 'REDE': {color: '#F39C12'}, 'SEM PARTIDO': {color: '#F2F3F4'}, 
                                                'SD': {color: '#F2F3F4'}, 'PRP': {color: '#F2F3F4'}, 'PTDOB': {color: '#F2F3F4'}, 
                                                'PSDC': {color: '#F2F3F4'}}",
                        hAxis = "{title: 'Maximum expenditure, per deputy (BRL)', minValue: 0, maxValue: 850000, 
                                  gridlines: {color: '#FAFAFA', count: 4}}",
                        vAxis = "{title: 'Average expenditure per deputy (BRL)', minValue: 0, maxValue: 550000, 
                                  gridlines: {color: '#FAFAFA', count: 5}}",
                        chartArea = "{left:150,top:70,width:750,height:350}",
                        explorer = "{}"
                        )
                      )
  })
  
  output$gastos_deputados <- renderPlotly({
  filter(reembolsos, deputy_name == input$deputado) %>%
    group_by(., year, receipt_description) %>%
    summarise(., total_spending = sum(value_receipts), max_receipt = max(value_receipts)) %>%
    ggplot(., aes(x = year, y = total_spending)) + geom_col(aes(fill = receipt_description)) + 
      theme(legend.text.align = 1) + theme_few() +  
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Pastel1"))(20)) + ylab("Average expenditure per category [R$]") +
      xlab("Year")
      
  })
  
  
  # show data using DataTable
  output$table <- DT::renderDataTable({
    datatable(filter(max_receipt, Category == input$category), rownames=FALSE) %>%
      formatStyle("Top Receipt [R$]",
                  background="skyblue", fontWeight='bold')
      })
  
  })



