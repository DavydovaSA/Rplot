# Работу выполнила ст. ИИС гр. ПМИ 4-1,
# Давыдова Светлана.

# Вариант 3
# Построить график разброса стоимости поставки
# от массы поставки в килограммах (исходные данные с 
# пропусками, переменная Netweight.kg), разделенный на панели
# (фасетки) по странам: Азербайджану, Республике Беларусь, 
# Казахстану. Добавить на график линии регрессии. Использовать
# пакет "ggplot2"

# загрузка пакетов
library("data.table")
library('ggplot2')

# загрузка файла с данными по импорту масла в РФ
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'

if (!file.exists('./data')) {
  dir.create('./data')
}
if (!file.exists('./data/download.log')) {
  file.create('./data/download.log')
}
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
  download.file(fileURL, './data/040510-Imp-RF-comtrade.csv')
  write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен', Sys.time()), 
        file = './data/download.log', append = T)
}
if (!exists('DT')){
  DT <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', as.is = T))
}

# отбир данных по странам согласно варианту задания
unique(DT$Reporter)
DT <- DT[ , United := '']
for (i in 1:nrow(DT))
  { if (DT$Reporter[i] == 'Azerbaijan')
  DT$United[i] <- DT$Reporter[i];
  if (DT$Reporter[i] == 'Belarus')
    DT$United[i] <- DT$Reporter[i];
if (DT$Reporter[i] == 'Kazakhstan')
  DT$United[i] <- DT$Reporter[i];

}
DT1 <- DT[-(1:15)]
DT2 <- DT1[-(11:24)]
DT3 <- DT2[-(23:27)]
DT4 <- DT3[-(35:42)]
DT5 <- DT4[-(45:54)]
DT6 <- DT5[-(57:79)]
DT7 <- DT6[-(69:75)]
DT8 <- DT7[-(81:87)]
DT9 <- DT8[-(93:97)]
DT10 <- DT9[-(115:125)]
DT11 <- DT10[-(127:143)]
DT12 <- DT11[-(134:141)]
DT13 <- DT12[-(141:141)]
DT <- DT13

# заполнение пропусков по переменной Netweight.kg средними значениями
DT[, Netweight.kg := as.double(Netweight.kg)]
DT[, round(median(.SD$Netweight.kg, na.rm = T), 0), 
   by = Year]
DT[, Netweight.kg.median := 
     round(median(.SD$Netweight.kg, na.rm = T), 0), 
   by = Year]
DT[!is.na(Netweight.kg), 
   Netweight.kg.median := Netweight.kg]
DT[, Netweight.kg, Netweight.kg.median]
DT[is.na(Netweight.kg), 
   Netweight.kg, Netweight.kg.median]
DT[, Trade.Value.USD:= as.double(Trade.Value.USD)]

# построение графика разброса и добавление линий регрессии
gp <- ggplot(data = DT,
             aes(x = Netweight.kg.median, y = Trade.Value.USD))
gp <- gp + geom_point()
gp <- gp + facet_grid(. ~ United)
gp <- gp + geom_smooth(method = 'lm')
# добавление подписей осей и заголовок
gp <- gp + xlab('Масса поставки') 
gp <- gp + ylab('Стоимость поставки')
gp <- gp + ggtitle('Зависимость стоимости поставки от массы поставки')

# сохранение графика
png('Rplot.png', width = 1000, height = 500)
plot(gp)
dev.off()
