
#songs list
m_page %>% html_nodes(".js-song-option") %>% html_text()
m_page %>% html_nodes(".js-song-option") %>% html_attr("value")
m_page %>% html_nodes(".js-song-option") %>% html_attr("data-albumdeliverykey") #Not Working

#albums list
m_page %>% html_nodes(".js-album-option") %>% html_text()
m_page %>% html_nodes(".js-album-option") %>% html_attr("value")
m_page %>% html_nodes(".js-album-option") %>% html_attr("data-background")
m_page %>% html_nodes(".js-album-option") %>% html_attr("data-albumdeliverykey") #Not Working

#venues list
m_page %>% html_nodes(".js-option-venue") %>% html_text()
m_page %>% html_nodes(".js-option-venue") %>% html_attr("value")

#tours list
m_page %>% html_nodes(".js-option-tour") %>% html_text()
m_page %>% html_nodes(".js-option-tour") %>% html_attr("value")

#cities list
m_page %>% html_nodes(".js-option-city") %>% html_text()
m_page %>% html_nodes(".js-option-city") %>% html_attr("value")
m_page %>% html_nodes(".js-option-city") %>% html_attr("data-state")
m_page %>% html_nodes(".js-option-city") %>% html_attr("data-country")
m_page %>% html_nodes(".js-option-city") %>% html_attr("data-continent")

#country list
m_page %>% html_nodes(".js-option-country") %>% html_text()
m_page %>% html_nodes(".js-option-country") %>% html_attr("value")
m_page %>% html_nodes(".js-option-country") %>% html_attr("data-country")
m_page %>% html_nodes(".js-option-country") %>% html_attr("data-continent")

#states list
m_page %>% html_nodes(".js-option-states") %>% html_text()
m_page %>% html_nodes(".js-option-states") %>% html_attr("value")
m_page %>% html_nodes(".js-option-states") %>% html_attr("data-country")
m_page %>% html_nodes(".js-option-states") %>% html_attr("data-continent")

#continents list
m_page %>% html_nodes(".js-option-continent") %>% html_text()
m_page %>% html_nodes(".js-option-continent") %>% html_attr("value")
m_page %>% html_nodes(".js-option-continent") %>% html_attr("data-continent")

#Decades Years list