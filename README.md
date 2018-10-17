# Аналитика по общественному транспорту Ростова-на-Дону и области

## Данные

Ключевым набором данных является перечень остановок общественного транспорта (ОТ) г.Ростова, опубликованный на сайте администрации города в разделе ["открытые данные"](http://rostov-gorod.ru/opendata/). Этот набор данных представляет из себя список всех актуальных остановок ОТ с названием, кординатами и списками маршрутов на них останавливающихся.

## Информационная часть

### График плотности остановок

Весь транспорт: 

![alt text](http://images.vfl.ru/ii/1539768802/e88b38a1/23840131_m.png "Весь транспорт")

Плотность маршрутов:

![alt text](http://images.vfl.ru/ii/1539768797/54768f39/23840130_m.png "Весь транспорт")

### Круговой график под новую схему районов.

Пересечение границ районов города со слоем остановок (в атрибутах которого есть данные по проходящим через остановку маршрутам) даёт новый массив с данными об однозначном отнесение остановки к тому или иному району.

Данные о маршрутах (автобусов) берутся исходя из того какие маршруты проходят через конкретную остановку, то есть нет маршрутов как последовательности точек на карте, а есть только набор остановок на каких останавливается конкретный маршрут (остановок с координатам и привязкой к "району"). Затем из этого набора остановок делается выжимка -- через какие районы маршрут проходит (исходя из уникальной привязки остановок к районам). В итоге получается таблица с двумя колонками (№ маршрута и районы). Массив (таблица) с остановками имеет такую структуру пересечения со слоем "районы": № маршрута, название остановки (все остановки где маршрут останавливается), координаты остановок, район, административный район и т.д.

В итоге можно построить таблицу связности район-район (с числом маршрутов). Именно по последней таблице строится круговой график. Если маршрутов соединяющих два района больше 0, то добавляется соответствующая линия. Её оттенок и толщина зависит от количество маршрутов.

![alt text](http://images.vfl.ru/ii/1539769110/b18fc047/23840171_m.png "Связность районов")
