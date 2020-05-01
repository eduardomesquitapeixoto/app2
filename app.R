library(shiny)

# which fields get saved 
fieldsAll <- c( 
  "HORA","id","entrevistador.identidade","comp","isolamentosocial","trabalho","trabalhomunicipio","meiodetransporte","mercado","servicosaude","parentes","lazer","impacto","impactosetores","risco","situacaoderua","abrigop","abrigoong","comodos","moradores","quemmora","renda","r1","r2","r3","r4","r5","cestaantes","cestadepois","sexo1","sxo2","sexo3","sexo4","sexo5","sexo6","sexo7","HIV","prep","prepredu","porquereduprep","TARV","TARVredu","porqueredutarv","Qualoutromotivoprep","Qualoutromotivotarv","bebida","alcool","maconha","cocaina","crack","tabaco","anfetamina","opioide","hipnoticos","alucinogenos","inalante","outradroga","outradroga2","habito1","habito2","habito3","ebento1","evento2","evento3","evento4","depre1","depre2","Q70.1","Q70.2","Q70.3","Q70.C","Q70.4","Q70.5","Q70.6","Q70.7","Q70.8","Q70.10","Q73.4","Q73.5","Q90","Q91","Q92","Q93","Q100","Q101","Q102","Q103","Q104","Q105","Q106","Q107","Q108","Q109","Q110","Q111","Q112","Q113","Q114","Q115","Q116","Q107","Q108","Q109","Q110","Q11","Q112","Q113","Q114","Q115","Q116","Q117","Q118","Q119","Q120","Q121","Q122","Q123","Q126","Q127","Q128","Q129","Q130","Q131","Q132","Q133","Q134","Q136","Q137","Q138","Q140","Q141","Q142","Q143","Q144","Q147","Q148","Q149","Q150","Q151","Q152","Q143","Q154","Q155","Q156","Q159","Q160","Q161","Q162","Q163","Q167","Q168"
  
 )

fieldsAll<-utf8::as_utf8(fieldsAll)
# which fields are mandatory
fieldsMandatory <- c( "id",
"entrevistador"	,
"HORA")

# add an asterisk to an input label
lAabelMandatory <- function(label) {
tagList(
label,
span("*", class = "mandatory_star")
)
}

# get current Epoch time
epochTime <- function() {
return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
fileName <- sprintf("%s_%s.csv",
humanTime(),
digest::digest(data))

write.csv(x = data, file = file.path(responsesDir, fileName),
row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
files <- list.files(file.path(responsesDir), full.names = TRUE)
data <- lapply(files, read.csv, stringsAsFactors = FALSE)
#data <- dplyr::rbind_all(data)
data <- do.call(rbind, data)
data
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

# usernames that are admins
adminUsers <- c("admin", "prof")

# info for sharing this app on facebook/twitter
share <- list(
title = "EVAS"

)

shinyApp(
ui = fluidPage(
shinyjs::useShinyjs(),
shinyjs::inlineCSS(appCSS),
title = "EVAS",
mainPanel(

# Output: Tabset w/ plot, summary, and table ----
tabsetPanel(type = "tabs", 
div(id = "header",
tabPanel("Bem-vindo(a)",
 h4("Projeto COVID-19: ") ) ,

img(src="https://00emp00.000webhostapp.com/Evas_Logo_Transparente.png", height = 400, width = 400)),

tabPanel("Questionário",
 
 
 fluidRow(
 column(12,
div(
id = "form",

h3("Questionário COVID"),

shinyjs::disabled( textInput("HORA", "Dia e Hora do início da Entrevista", value = Sys.time())),
 textInput("id", "Prontuário"),

 textInput("entrevistador", "entrevistador"),
 
selectInput("identidade","Como você se define?",c("","Mulher cis", "travesti","mulher transexual não redesignada", "mulher trans redesignada")),

 
 
 h3("Grau de Isolamento"),
h4( "Seja muito Bem vinda, vamos começar..."),


selectInput("comp","Você tem ou teve companheiro fixo nos ´últimos 12 meses?",c("","Sim","Não","não quero responder / não sei")),

 selectInput("isolamentosocial","Você está em isolamento social?",c("","sim - total","sim - parcial", "não","não quero responder / não sei")),

 selectInput("trabalho","Você está trabalhando?",c("","sim - em casa","sim - fora de casa com o mesmo número de horas de trabalho", "sim - fora de casa com jornada de trabalho reduzida","sim - fora de casa com jornada de trabalho aumentada/residindo no trabalho","Dispensada por ora do trabalho, com manutenção da remuneração","Dispensada por ora do trabalho, sem manutenção da remuneração","Demitida","Não","não quero responder / não sei")),
 
 conditionalPanel(condition =  "input.trabalho != 'sim' && input.trabalho!='Não' && input.trabalho!= 'Demitida'",
 selectInput("trabalhomunicipio","Você trabalha no mesmo município da sua moradia?",c("","sim", "não","não quero responder / não sei")),
 
 selectInput("meiodetransporte","Qual seu meio de transporte até o trabalho?",c("","Bicicleta","Moto","Carro", "Ônibus","Metrô","Trem","Morando no trabalho","Não sei / não quero responder"))),
 selectInput("mercado","Você está indo ao mercado:",c("","sim - normalmente","sim - o mínimo possível","Não (se suas compras estiverem sendo feitas por internet, telefone ou outra pessoa)" ,"Não sei/ não quero responder")),
 
 selectInput("servicosaude","Você está indo à centros de saúde, hospitais ou farmácias:",c("","sim - normalmente","sim - o mínimo possível","Não","Não sei / Não quero responder")),
 
 selectInput("parentes","Você está indo à casa de outras pessoas (parentes, amigos, clientes)?",c("","sim - normalmente","sim - o mínimo possível","Não","Não sei / Não quero responder")),
 selectInput("lazer","Você está indo a áreas de lazer?",c("","sim - normalmente","sim - o mínimo possível","Não","Não sei / Não quero responder")),
 
selectInput("impacto","De modo geral, como você classificaria o impacto que as recomendações de isolamento social tiveram na sua vida?",c("","Muito impacto - toda a rotina diária foi modificada.","Médio impacto - apenas parte da rotina diária foi modificada.","Pouco impacto - precisou modificar poucas coisas da rotina diária.","Nenhum impacto - não modificou nada da rotina diária.","Não sabe avaliar o impacto.","Não quis responder")),
selectInput("impactosetores","Qual setor da sua vida foi mais afetado pelas recomendações de isolamento social?", c("","Financeiro/econômico (incluindo trabalho)","Afetivo/Sexual","Familiar","Amizades","Nenhum setor na minha vida foi afetado","outro")),
textInput("outrosetor","Outro setor"),
selectInput("risco","Como você se sente em relação ao seu risco de contrair o novo coronavírus?",c("","Tem muito medo","Tem medo moderado","Tem pouco medo","Não tem medo","Já pegou o novo coronavírus")),


 h3("Condições de Moradia"),
 selectInput("situacaoderua","Você está em situação de rua?",c("","Sim","Não","não quero responder / não sei")),
 selectInput("abrigop","Você está morando em abrigamento público?",c("","Sim","Não","não quero responder / não sei")),
selectInput("abrigoong","Você está morando em abrigamento de ONGS?",c("","","Sim","Não","não quero responder / não sei")),
 textInput("comodos","Quantos cômodos tem o local onde você está ficando?"),
 textInput("moradores","Quantas pessoas estão dividindo o espaço com você?"),
conditionalPanel(condition = "input.moradores!='0'",
selectInput("quemmora", "Com quem você mora?", c("pai","mãe", "companheiro(a)","cafetina","Amigos","irmãos/ tios ou avós","cafetina","sozinha","não quero responder / não sei"),multiple=T)),

h3("Condições de Renda"),

h4("responda em números inteiros, deixe em branco caso não se aplique"),

textInput("renda","Qual era o seu rendimento mensal normalmente recebido nos últimos 12 meses? (considere a média de todos os rendimentos de trabalho, aposentadoria, bolsa família ou outra origem, nos últimos 12 meses)"),
selectInput("r1", "outras informações sobre renda pré-isolamento:", c("","recebo fontes alternativas de renda, como comida e abrigo", "não recebo nenhuma renda", "não se aplica")),

textInput("r2","Qual está sendo o seu rendimento mensal normalmente recebido desde o início da recomendação de isolamento social? (considere a média de todos os rendimentos de trabalho, aposentadoria, bolsa família ou outra origem, nos últimos 2 meses)"),
selectInput("r3", "outras informações sobre renda pós-isolamento:", c("","recebo fontes alternativas de renda, como comida e abrigo", "não recebo nenhuma renda",  "Recebo auxílio para trabalhadores informais", "não se aplica")),
conditionalPanel(condition = "input.moradores != '0'",
textInput("r4","Quantas pessoas que dividem espaço contigo tem renda de alguma a natureza?"),
textInput("r5","Qual a renda somada das pessoas que dividem o espaço contigo na quarentena?")),

h3("Segurança Alimentar"),

selectInput("cestaantes","ANTES DO ISOLAMENTO SOCIAL, você precisou de cesta básica e/ ou doações para que pudesse se alimentar?
?", c("","sim", "não", "não sabe ou não quis responder" )),

selectInput("cestadepois","DURANTE O ISOLAMENTO SOCIAL, você precisou de cesta básica e/ ou doações para que pudesse se alimentar?
?", c("","sim", "não", "não sabe ou não quis responder" )),

h3("Comportamentos de Risco Sexual"),

conditionalPanel(condition = "input.identidade =='Mulher cis' || input.identidade=='mulher trans redesignada'",

selectInput("sexo1","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, você fez sexo vaginal sem camisinha com algum parceiro/a que possuísse pênis?", c("","sim", "não", "não sabe ou não quis responder" ))),


selectInput("sexo2","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL você trabalhou como profissional do sexo ou trocou sexo por dinheiro, favores, presentes ou drogas?", c("","sim", "não", "não sabe ou não quis responder" )),

selectInput("sexo3","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL você fez sexo anal com algum parceiro/a que possuísse pênis?", c("","sim", "não", "não sabe ou não quis responder" )),

selectInput("sexo4","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, você fez sexo anal sem camisinha com algum parceiro/a que possuísse pênis?", c("","sim", "não", "não sabe ou não quis responder" )),

selectInput("sexo5","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL você teve alguma parceria sexual que era sabidamente HIV-positiva?", c("","sim", "não", "não sabe ou não quis responder" )),

selectInput("sexo6","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL você praticou sexo virtual?", c("","sim", "não", "não sabe ou não quis responder" )),

selectInput("sexo7","Você é HIV-positiva?", c("","sim", "não", "não sabe ou não quis responder" )),


conditionalPanel(condition = "input.HIV=='não'",

selectInput("prep","Você está em uso de PrEP?", c("","sim - diariamente","sim - mas não diariamente", "não", "não sabe ou não quis responder" )),
selectInput("prepredu","Você interrompeu ou reduziu o uso do PrEP após o isolamento social?", c("","sim", "não", "não sabe ou não quis responder" )),
selectInput("porquereduprep","Porque você reduziu?", c("","não se aplica","Sobrecarregada com casa/filhos. Não tenho tempo/esqueço com mais frequência de tomar", "medo de ser vista tomando o remédio", "medo de ir a FIOCRUZ pegar o remédio", "Dificuldade para ir à Fiocruz para pegar PrEP (transporte interrompido, falta de dinheiro para transporte)","Diminui/interrompi meus relacionamentos sexuais/ Me vejo em menor risco","outro motivo",
"não sabe ou não quis responder" ), multiple = T),
textInput("Qualoutromotivoprep","Outro Motivo")),


conditionalPanel(condition = "input.HIV=='sim'",
                 
selectInput("TARV","Você está em uso da TARV?", c("","sim - diariamente","sim - mas não diariamente", "não", "não sabe ou não quis responder" )),
selectInput("TARVredu","Você interrompeu ou reduziu o uso da TARV após o isolamento social?", c("","sim", "não", "não sabe ou não quis responder" )),
selectInput("porqueredutarv","Porque você reduziu?", c("","não se aplica", "Sobrecarregada com casa/filhos. Não tenho tempo/esqueço com mais frequência de tomar","medo de ser vista tomando o remédio", "medo de ir a FIOCRUZ pegar o remédio", "Dificuldade para ir à Fiocruz para pegar TARV (transporte interrompido, falta de dinheiro para transporte)","Diminui/interrompi meus relacionamentos sexuais/ Me vejo em menor risco","outro motivo","não sabe ou não quis responder"), multiple = T)
                                                       

),

h3("Saúde Mental"),



selectInput("bebida","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você tomou 5 ou mais doses de bebida alcoólica em um período de duas horas? (Isso seria o equivalente a pelo menos 5 latas de cerveja ou 5 taças de vinho ou 5 copos de cachaça).", c("","Todos os dias","Em quase todos os dias","Em mais da metade dos dias","Em menos da metade dos dias","Bebi, mas menos que 5 doses","não bebi neste período")),
selectInput("alcool","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou álcool", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("maconha","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou maconha", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("cocaina","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou cocaína", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("crack","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou crack", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("tabaco","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou tabaco", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("anfenatamina","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou ANFETAMINAS (CRISTAL OU SPEED) OU ÊXTASE, MDOU BALA", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("opioide","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou OPIÓIDES (HEROÍNA, MORFINA)", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("hipnoticos","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou HIPNÓTICOS/SEDATIVOS (REMÉDIOS PARA DORMIR SEM PRESCRIÇÃO)", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("alucinogenos","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou ALICINÓGENOS (LSD, ÁCIDO, CHÁ DE COGUMELO", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("inalante","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou INALANTES (LOLÓ, TINER)", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
selectInput("outradroga","DURANTE O PERÍODO DE ISOLAMENTO SOCIAL, com que frequência você utilizou outras drogas", c("","Nunca"," 1 ou 2 Vezes","Pelo menos uma vez por mês","Semanalmente","diariamente (quase todos os dias")),
textInput("outradroga2", "Qual?"),

selectInput("habito1","Durante o período do isolamento social, como você se considera em relação ao hábito de fumar", c("","Continuou sem fumar","Começou a fumar","Parou de fumar","Usou menos","usou mais","não sabe \ não quer responder" )),
selectInput("habito2","Durante o período do isolamento social, como você se considera em relação ao consumo de bebidas alcoólicas:", c("","Continuou sem beber","Começou a beber","Parou de beber","Usou menos","usou mais","não sabe \ não quer responder" )),
selectInput("habito3","Durante o período do isolamento social, como você se considera em relação ao uso das drogas",c("","Continuou sem usar","Começou a usar","Parou de usar","Usou menos","usou mais","não sabe \ não quer responder" )),
h4("Às vezes, acontecem coisas com pessoas que são incomuns ou especialmente assustadoras, horríveis ou traumáticas. Por exemplo: um acidente ou incêndio grave; um ataque ou abuso físico ou sexual; um terremoto ou inundação; uma guerra; ver alguém ser morto ou gravemente ferido; ver um ente querido morrer por homicídio ou suicídio."),
selectInput("evento1","Você já experimentou esse tipo de evento?", c("","sim","não","não quis responder" )),
h4("No último mês..."),
selectInput("evento2","Por favor, indique o quanto você foi incomodada por estes problemas durante o último mês.", c("","nada","um pouco", "médio", "bastante", "muito" )),
selectInput("evento3","Memórias, pensamentos e imagens repetitivos e perturbadores referentes a uma experiência estressante do passado:",c("","nada","um pouco", "médio", "bastante", "muito" )),
selectInput("evento4","Sentir-se muito chateada ou preocupada quando alguma coisa lembra você de uma experiência estressante do passado:",c("","nada","um pouco", "médio", "bastante", "muito")),
h4("Durante os últimos 14 dias, em quantos foi afetada por algum dos seguintes problemas?"),

selectInput("depre1","Tive pouco interesse ou prazer em fazer coisas:",c("", "nunca", "em vários dias", "em mais da metade dos dias", "em quase todos os dias")),
selectInput("depre2",c("Senti desânimo, desalento ou falta de esperança"),c("","nunca", "em vários dias", "em mais da metade dos dias", "em quase todos os dias")),

h3("Experiências com a violência comunitária"),

 h4(" Reforçarque não temos relações com instituições militares e que nossa intenção é apenas entender as vivências delas com o fenômeno da violência;"),
h4(" 
 Entrevistador: “Agora eu vou perguntar sobre algumas situações que podem ser consideradas
 difíceis, mas que fazem parte do dia a dia de algumas pessoas. Caso você se sinta muito incomodada,
 por favor, pode falar e até mesmo interromper a entrevista. Mas lembro que esta parte do instrumento
 é muito importante para que possamos compreender melhor a vida das pessoas e para que possamos
 gerar dados capazes de subsidiar a construção de políticas públicas mais eficazes.
 Começaremos sobre sua vida atual na sociedade e depois vamos perguntar algumas coisas sobre a sua
 infância. Quero lembrar que tudo que você me responder estará em segredo de pesquisa e que estes
 dados não serão divulgados de modo que possam te identificar”


 
   Alguma vez na vida...
   ") ,

selectInput("Q70.1","alguém feriu você com arma de fogo? (1) sim - uma vez, (2) sim - algumas vezes,
(3) sim - muitas vezes, (0) não - (99) não sabe, não lembra, não respondeu		", c("", "0"," 1", "2"," 3", "4"," 99")),	
selectInput("Q70.2","	alguém lhe ofereceu drogas? (1) sim - uma vez, (2) sim - algumas vezes, (3) sim - muitas vezes, (0) não - (99) não sabe, não lembra, não respondeu	", c("", "0"," 1", "2"," 3", "4"," 99")),	
selectInput("Q70.3","	você sentiu necessidade de andar com arma de fogo? (1) sim - uma vez, (2) sim - algumas vezes, (3) sim - muitas vezes, (0) não - (99) não sabe, não lembra, não respondeu		", c("", "0"," 1", "2"," 3", "4"," 99")),	
selectInput("Q70c","	você sentiu necessidade de andar com arma branca? (1) sim - uma vez, (2) sim - algumas vezes, (3) sim - muitas vezes, (0) não - (99) não sabe, não lembra, não respondeu		", c("", "0"," 1", "2"," 3", "4"," 99")),	

selectInput("Q70.4","você ou algum(a) parente próximo foi ameaçada de morte? (1) sim - uma vez, (2) sim - algumas vezes, (3) sim - muitas vezes, (0) não - (99) não sabe, não lembra,
não respondeu			", c("", "0"," 1", "2"," 3", "4"," 99")),	
selectInput("Q70.5"," algum desconhecido a agrediu fisicamente (tapa, soco, etc)? (1) sim - uma vez, (2)
sim - algumas vezes, (3) sim - muitas vezes, (0) não - (99) não sabe, não lembra, não respondeu		", c("", "0"," 1", "2"," 3", "4"," 99")),	
selectInput("Q70.6","	algumamigo(a) ou colega a agrediu fisicamente?(1)sim -umavez,(2)sim - algumas vezes, (3) sim - muitas vezes, (0) não - (99) não sabe, não lembra, não
respondeu			", c("", "0"," 1", "2"," 3", "4"," 99")),	
selectInput("Q70.7","	algum membro da família a agrediufisicamente?(1)sim - umavez,(2)sim -
algumas vezes, (3) sim - muitas vezes, (0) não - (99) não sabe, não lembra, não respondeualgummembrodafamíliaaagrediufisicamente?(1)sim -umavez,(2)sim -
", c("", "0"," 1", "2"," 3", "4"," 99")),	
selectInput("Q70.8","	você já sofreu atentado de morte nos últimos doze meses? (0) Não; (1) sim		", c("", "0"," 1", "99")),	
selectInput("Q70.9","	você já sofreu atentado de morte alguma vez na vida? (0) Não; (1) sim	", c("", "0"," 1"," 99")),	

selectInput("Q70.10","alguém feriu você com arma branca (1) sim - uma vez, (2) sim - algumas vezes,
(3) sim - muitas vezes, (0) não - (99) não sabe, não lembra, não respondeu		", c("", "0"," 1", "2"," 3", "4"," 99")),
h4("Em que locais ou situações você se sentiu rejeitada ou discriminada?"), 
selectInput("Q73.4","em local público fechado (banco, shopping, loja, restaurante, boate, clube, igreja, órgão público, etc (1) uma vez, (2) algumas vezes, (3) muitas vezes, (0) nunca,
(99) não sabe/não respondeu			", c("", "0"," 1", "2"," 3", "4"," 99")),	
selectInput("Q73.5","	em local público aberto (rua, praça, parque, jardim, praia, etc) (1) uma vez, (2) algumas vezes, (3) muitas vezes, (0) nunca, (99) não sabe/não respondeu			", c("", "0"," 1", "2"," 3", "4"," 99")),	

conditionalPanel(    condition = "input.comp == 'Sim'", 
                     
h3("Violência de Parceiro Íntimo"),
h4("
 MÓDULO III.5 – Revised Confict Tactics Scales
 Entrevistador, leia:
 “Mesmo que um casal se relacione bem, tem vezes em que um discorda do outro, se chateia
 com o outro, quer coisas diferentes ou discutem e se agridem apenas porque estão de mau
 humor, cansados ou por outra razão qualquer. Os casais também têm maneiras diferentes de
 tentar resolver seus problemas. Esta é uma lista de coisas que podem acontecer quando
 existem diferenças ou desavenças entre um casal. Por favor, eu gostaria de saber se você e
 seu/sua (ex)companheiro/a fizeram cada uma dessas coisas. Para cada uma das coisas que eu
 vou dizer a seguir, eu gostaria que você me dissesse se já aconteceu nos últimos 12 meses e
 alguma vez na sua vida”.
 “Diante de uma desavença ou discussão entre você e seu/sua (ex)companheiro(a) ...”"),
selectInput("Q90","Você insultou ou xingou o seu companheiro(a)? 	", c("", "sim"," não", "NA")),	
selectInput("Q91","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim	", c("", "sim"," não", "NA")),	
 selectInput("Q92","Q92	Você jogou alguma coisa no seu companheiro(a) que poderia machucá-lo?
 ", c("", "sim"," não", "NA")),	
selectInput("Q93","	Seu companheiro(a) fez isso com você?	", c("", "sim"," não", "NA")),	
selectInput("Q100","Você obrigou o seu companheiro(a) a fazer sexo sem usar camisinha? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q101","	Seu companheiro(a) fez isso com você? 		", c("", "sim"," não", "NA")),	
selectInput("Q102","	Você deu um empurrão no seu companheiro(a)? (0)Não; (1)simVocê deu um empurrão no seu companheiro(a)? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q103","Seu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q104","Você usou de força como, por exemplo, segurar ou bater nele ou usar uma arma, para obrigar o seu companheiro(a) a fazer sexo oral ou anal com você? (	", c("", "sim"," não", "NA")),	
 selectInput("Q105","	Seu companheiro(a) fez isso? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q106","	Você usou uma faca ou arma contra o seu companheiro(a)? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q107","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim	", c("", "sim"," não", "NA")),	
selectInput("Q108"," Você desmaiou ao levar uma pancada na cabeça durante uma briga com o seu companheiro(a)? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q109","	Seu companheiro(a) desmaiou ao levar uma pancada na cabeça durante uma briga com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q110","	Você chamou o seu companheiro(a) de gordo/a, feio/a ou alguma coisa parecida? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q111","	Seu companheiro(a) chamou você de gorda/o, feia/o ou alguma coisa parecida?
", c("", "sim"," não", "NA")),	
selectInput("Q112","	Você deu um murro ou acertou o seu companheiro(a) com alguma coisa que pudesse machucar? ", c("", "sim"," não", "NA")),	
selectInput("Q113","	Seu companheiro(a) fez isso com você? (0)Não; (1)simSeu companheiro(a) fez isso com você? (0)Não; (1)simSeu companheiro(a) fez isso com você? (0)Não; (1)simSeu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q114","	Você destruiu alguma coisa que pertencia ao seu companheiro(a) de propósito?			", c("", "sim"," não", "NA")),	
selectInput("Q115","	Seu companheiro(a) fez isso? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q116","	Você foi a um médico ou serviço de saúde por causa de uma briga com seu companheiro? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q117","	Seu companheiro(a) foi ao médico ou algum serviço de saúde por causa de uma briga com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q118","	Você sufocou ou estrangulou seu companheiro(a)? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q119","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q120","	Você gritou ou berrou com o seu companheiro(a)? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q121","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q122","	Você jogou o seu companheiro(a) contra a parede com força? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q123","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim	", c("", "sim"," não", "NA")),	
selectInput("Q126","	Você deveria ter ido a um médico ou algum serviço de saúde por causa de uma briga com seu companheiro, mas não foi? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	 
selectInput("Q127"," Seu companheiro(a) deveria ter ido a um médico ou algum serviço de saúde por causa de uma briga com você, mas não foi? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q128","	Você deu uma surra no seu companheiro(a)? (0)Não; (1)sim		", c("", "sim"," não", "NA")),	
selectInput("Q129","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q130","	Você segurou o seu companheiro(a) com força? (0)Não; (1)sim", c("", "sim"," não", "NA")),	
selectInput("Q131","	Seu companheiro(a) fez isso com você?		", c("", "sim"," não", "NA")),	
selectInput("Q132","	Você usou de força como, por exemplo, segurar ou bater nele ou usar uma arma para obrigar o seu companheiro(a) a fazer sexo com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q133"," Seu companheiro(a) fez isso? 	", c("", "sim"," não", "NA")),	
selectInput("Q134", " Você virou as costas e foi embora no meio de uma discussão? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q135"," Seu companheiro(a) fez isso? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q136","	 Você insistiu em fazer sexo quando o seu companheiro(a) não queria sem usar força física? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q137"," Seu companheiro(a) fez isso com você?		", c("", "sim"," não", "NA")),	
selectInput("Q138"," Você deu um tabefe ou bofetada no seu companheiro(a)? 		", c("", "sim"," não", "NA")),	
selectInput("Q140"," Seu companheiro(a) fez isso com você? (0)Não; (1)simSeu companheiro(a) fez isso com você? (0)Não; (1)simSeu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q141"," Você quebrou um osso por causa de uma briga com o seu companheiro(a)?
(0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q142","	Seu companheiro(a) quebrou um osso por causa de uma briga com você?
(0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q143","	Você fez ameaças para obrigar o seu companheiro(a) fazer sexo oral ou anal com você? 			", c("", "sim"," não", "NA")),	
selectInput("Q144","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q147","	Você queimou ou derramou líquido quente em seu companheiro(a) de propósito? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q148","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q149","	Você insistiu para que seu companheiro(a) fizesse sexo oral ou anal com você sem usar força física? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q150","	Seu companheiro(a) fez isso com você? 		", c("", "sim"," não", "NA")),	
selectInput("Q151","	Você acusou o seu companheiro(a) de ser “ruim de cama”? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q152","	Seu companheiro(a) acusou-o(a) disso? 	", c("", "sim"," não", "NA")),	
selectInput("Q153","	Você fez alguma coisa para ofender o seu companheiro(a)? (0)Não; (1)sim", c("", "sim"," não", "NA")),	
selectInput("Q154","	Seu companheiro(a) fez isso? (0)Não; (1)sim	", c("", "sim"," não", "NA")),	
selectInput("Q155","	Você ameaçou acertar ou jogar alguma coisa no seu companheiro(a)? (0)Não; (1)sim	", c("", "sim"," não", "NA")),	
selectInput("Q156"," Seu companheiro(a) fez isso? (0)Não; (1)sim", c("", "sim"," não", "NA")),	
selectInput("Q159","	Você chutou o seu companheiro(a)?			", c("", "sim"," não", "NA")),	
selectInput("Q160","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q161","	Você fez ameaças para obrigar o seu companheiro(a) fazer sexo com você?
(0)Não; (1)sim			", c("", "sim"," não", "NA")),	
selectInput("Q162","	Seu companheiro(a) fez isso com você? (0)Não; (1)sim			", c("", "sim"," não", "NA")),
 selectInput("Q163","	Você julga que sofre mais violência com o isolamento social		", c("", "sim"," não", "NA"))),

textInput("Q167","Horário de Término", c("", "sim"," não", "NA")),

selectInput("Q168","ENTREVISTADOR: Houve respeito a privacidade da respondente, ou seja, a informante foi entrevistada: (1) Sozinha; (2) Na presença do companheiro; (3)
Na presença de filhos ou pais; (4) Na presença de outras pessoas – Marque e
", c("", "1"," 2", "3","4","NA" )),	

textInput("Q169","Q169	OBSERVAÇÕES		"),

actionButton("submit", "Submit", class = "btn-primary"),

shinyjs::hidden(
span(id = "submit_msg", "Submitting..."),
div(id = "error",
div(br(), tags$b("Error: "), span(id = "error_msg"))
)
)
))),
 
 shinyjs::hidden(
 div(
 id = "thankyou_msg",
 h4("Arrasou, Obrigado por Participar!"),
 actionLink("submit_another", "Submit another response")
 )
 )
),

tabPanel("Dados",
 column(6,
uiOutput("adminPanelContainer")
 )
),
tabPanel("", 
 
 # Define UI for application that draws a histogram
 shinyUI(fluidPage(
 
 )))
))),
server = function(input, output, session) {


# Enable the Submit button when all mandatory fields are filled out
observe({
mandatoryFilled <-
vapply(fieldsMandatory,
 function(x) {
 !is.null(input[[x]]) && input[[x]] != ""
 },
 logical(1))
mandatoryFilled <- all(mandatoryFilled)

shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
})

# Gather all the form inputs (and add timestamp)
formData <- reactive({
data <- sapply(fieldsAll, function(x) input[[x]])
data <- c(data, timestamp = epochTime())
data <- t(data)
data
})

# When the Submit button is clicked, submit the response
observeEvent(input$submit, {

# User-experience stuff
shinyjs::disable("submit")
shinyjs::show("submit_msg")
shinyjs::hide("error")

# Save the data (show an error message in case of error)
tryCatch({
saveData(formData())
shinyjs::reset("form")
shinyjs::hide("form")
shinyjs::show("thankyou_msg")
},
error = function(err) {
shinyjs::html("error_msg", err$message)
shinyjs::show(id = "error", anim = TRUE, animType = "fade")
},
finally = {
shinyjs::enable("submit")
shinyjs::hide("submit_msg")
})
})

# submit another response
observeEvent(input$submit_another, {
shinyjs::show("form")
shinyjs::hide("thankyou_msg")
})

# render the admin panel
output$adminPanelContainer <- renderUI({
if (!isAdmin()) return()

div(
id = "adminPanel",
h4("BANCO DE DADOS"),
downloadButton("downloadBtn", "Download responses"), br(), br(),
DT::dataTableOutput("responsesTable") 
)
})

# determine if current user is admin
isAdmin <- reactive({
is.null(session$user) || session$user %in% adminUsers
})

# Show the responses in the admin table
output$responsesTable <- DT::renderDataTable(
loadData(),
rownames = FALSE,
options = list(searching = TRUE, lengthChange = FALSE)
)

# Allow user to download responses
output$downloadBtn <- downloadHandler(
filename = function() { 
sprintf("mimic-google-form_%s.csv", humanTime())
},
content = function(file) {
write.csv(loadData(), file, row.names = FALSE)
}
)





 

}
)



