# TODO: Add comment
# 
# Author: pacha
###############################################################################
library(lattice)
setwd("/Users/pacha/Dropbox/R/RProgramming/tarea3")
Outcome <- read.csv("Outcome-of-care-measures.csv", colClasses = "character")
head(Outcome)

#PARTE 1
#1	Plot the 30-day mortality rates for heart attack
Outcome[, 11] <- as.numeric(Outcome[, 11])  
hist(Outcome[, 11],xlab="30-day Death Rate",main="Heart Attack 30-day Death Rate")                                                                 

#PARTE 2
#2	Finding the best hospital in a state
Init <- function(fileStr, workDirStr = "/Users/pacha/Dropbox/R/RProgramming/tarea3") {
	retDfr <- read.csv(fileStr, colClasses = "character")
	return(retDfr)
}

best <- function(stateChr, outcomeChr) {
	#abrir archivo
	outcomeDfr <- Init("Outcome-of-care-measures.csv")
	#eliminar warnings y dejar todo en formato numerico
	suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11]))
	suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17]))
	suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23]))
	#crear tabla de frecuencia por estado y sacar row.names
	tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
							length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
	rownames(tableDfr) <- NULL
	#crear tabla de inputs y columnas respectivas
	inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
			Col = c(11, 17, 23))
	#verificar que los inputs son validos
	if (nrow(tableDfr[tableDfr$State == stateChr, ]) == 0) 
		stop("invalid state")
	if (nrow(inputDfr[inputDfr$Outcome == outcomeChr, ]) == 0) 
		stop("invalid outcome")
	#encontrar hospital de menor mortandad ultimo mes en un estado dado
	stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
	colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
	rowNum <- which.min(stateDfr[, colNum])
	#mostrar mejor hospital de un estado
	return(stateDfr[rowNum, ]$Hospital.Name)
}

#PARTE 3
#3	Ranking hospitals by outcome in a state
rankhospital <- function(stateChr, outcomeChr, rankObj) {
	#abrir archivo
	outcomeDfr <- Init("Outcome-of-care-measures.csv")
	#eliminar warnings y dejar todo en formato numerico
	suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11]))
	suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17]))
	suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23]))
	#crear tabla de frecuencia por estado y sacar row.names
	tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
							length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
	rownames(tableDfr) <- NULL
	#crear tabla de inputs y columnas respectivas
	inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
			Col = c(11, 17, 23))
	#verificar que los inputs son validos
	if (nrow(tableDfr[tableDfr$State == stateChr, ]) == 0) 
		stop("invalid state")
	if (nrow(inputDfr[inputDfr$Outcome == outcomeChr, ]) == 0) 
		stop("invalid outcome")
	#armar ranking por estado
	stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
	colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
	stateDfr <- stateDfr[complete.cases(stateDfr[, colNum]), ]
	stateDfr <- stateDfr[order(stateDfr[, colNum], stateDfr$Hospital.Name), 
	]
	#convertir ranking a formato numerico y omitir NAs
	if (rankObj == "best") 
		rankObj <- 1
	if (rankObj == "worst") 
		rankObj <- nrow(stateDfr)
	suppressWarnings(rankNum <- as.numeric(rankObj))
	#ranking por estado
	return(stateDfr[rankNum, ]$Hospital.Name)
}

#PARTE 4
#4	Ranking hospitals in all states
rankall <- function(outcomeChr, rankObj = "best") {
	#abrir archivo
	outcomeDfr <- Init("Outcome-of-care-measures.csv")
	#eliminar warnings y dejar todo en formato numerico
	suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11]))
	suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17]))
	suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23]))
	#crear tabla de frecuencia por estado y sacar row.names
	tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
							length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
	rownames(tableDfr) <- NULL
	#crear tabla de inputs y columnas respectivas
	inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
			Col = c(11, 17, 23))
	#verificar que los inputs son validos
	if (nrow(inputDfr[inputDfr$Outcome == outcomeChr, ]) == 0) 
		stop("invalid outcome")
	#agregar vector nulo para escribir el ranking sobre las componentes
	nameChr <- character(0)
	#construir ranking global
	for (stateChr in tableDfr$State) {
		stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
		colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
		stateDfr <- stateDfr[complete.cases(stateDfr[, colNum]), ]
		stateDfr <- stateDfr[order(stateDfr[, colNum], stateDfr$Hospital.Name), 
		]
		#convertir ranking a formato numerico y omitir NAs
		if (rankObj == "best") 
			rankNum <- 1 else if (rankObj == "worst") 
			rankNum <- nrow(stateDfr) else suppressWarnings(rankNum <- as.numeric(rankObj))
		#reemplazar coordenadas del vector nulo
		nameChr <- c(nameChr, stateDfr[rankNum, ]$Hospital.Name)
	}
	#ranking global
	return(data.frame(hospital = nameChr, state = tableDfr$State))
}

#prueba parte 2
best("TX", "heart attack") #"CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure") #"FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack") #"JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia") #"GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack") #Error in best("NY", "hert attack") : invalid state
best("NY", "hert attack") #Error in best("NY", "hert attack") : invalid outcome

#prueba parte 3
rankhospital("TX", "heart failure", 4) #"DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst") #"HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000) #NA

#prueba parte 4
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

#envio
source("rprog_scripts_submitscript3.R")
submit()