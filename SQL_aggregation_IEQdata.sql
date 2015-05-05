Select * from 5min_room_with_boxes;

Select count(*) from 5min_room_with_boxes;

show columns from 5min_room_with_plants;

drop table 5min_room_with_boxes; 
drop table 5min_room_with_plants; 

drop view 5min_RB;
drop view 5min_RP;
drop view 5min_IEQ;

create view 5min_RB as 
SELECT   Timestamp as "Timestamp", Room as "Room", avg(CO) as "CO",avg(CO2) as "CO2",
avg(Humidity) as "Humidity",avg(Light) as "Light",avg(NO2) as NO2,avg(O2) as "O2", avg(PM) as "PM", avg(Sound) as "Sound",
avg(Temperature) as "Temperature", avg(TVOC) as "TVOC", ROUND((UNIX_TIMESTAMP(Timestamp)+30)/(300)) AS timekey
FROM  5min_room_with_boxes		
GROUP BY timekey
;

create view 5min_RP as 
SELECT   Timestamp as "Timestamp", Room as "Room", avg(CO) as "CO",avg(CO2) as "CO2",
avg(Humidity) as "Humidity",avg(Light) as "Light",avg(NO2) as NO2,avg(O2) as "O2", avg(PM) as "PM", avg(Sound) as "Sound",
avg(Temperature) as "Temperature", avg(TVOC) as "TVOC", ROUND((UNIX_TIMESTAMP(Timestamp)+30)/(300)) AS timekey
FROM  5min_room_with_plants		
GROUP BY timekey
;

Create view 5min_IEQ as 
SELECT * FROM 5min_RB
UNION
SELECT * FROM 5min_RP
;

Select * from 5min_IEQ;

drop view 5min_reg;

[1] "Timestamp"   "Room"        "P_ID"        "Activity"    "CO"          "CO2"         "Humidity"   
 [8] "Light"       "NO2"         "O2"          "PM"          "Sound"       "Temperature" "TVOC"       
[15] "HR"          "RR"          "SDNN"        "RMSSD"       "pNN50"       "VLF"         "LF"         
[22] "HF"          "VLFper"      "LFper"       "HFper"       "LFHF"    

create view 5min_reg as 
SELECT   Timestamp as "Timestamp", Room as "Room", avg(CO) as "CO",avg(CO2) as "CO2",
avg(Humidity) as "Humidity",avg(Light) as "Light",avg(NO2) as NO2,avg(O2) as "O2", avg(PM) as "PM", avg(Sound) as "Sound",
avg(Temperature) as "Temperature", avg(TVOC) as "TVOC", 
avg(HR) as "HR",avg(RR) as "RR", avg(LF) as "LF", avg(HF) as "HF", avg(SDNN) as"SDNN",avg(RMSSD) as "RMSSD", avg(pNN50) as "pNN50", 
avg(VLF) as "VLF",avg(VLFper) as "VLFper", avg(LFper) as "LFper",avg(HFper) as "HFper",avg(LFHF) as "LFHF",
ROUND((UNIX_TIMESTAMP(Timestamp)+30)/(300)) AS timekey
FROM  5min_regression		
GROUP BY timekey,Room
;

Select * from 5min_reg;