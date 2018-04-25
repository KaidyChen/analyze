#!/bin/bash
FIELDS="eqpt_info.eqpt_type, eqpt_info.eqpt_code, network_config.upper_eqpt_id_code, eqpt_info.eqpt_building_id, 
eqpt_info.eqpt_label"
SQL="SELECT ${FIELDS} FROM eqpt_info, network_config, eqpt_type WHERE eqpt_info.eqpt_code = network_config.eqpt_id_code AND 
eqpt_info.eqpt_type = eqpt_type.eqpt_type AND eqpt_type.eqpt_class = 'meter'"
#echo $SQL
sqlite3 $1 "$SQL" | awk '
	BEGIN {FS="|"; OFS="#"}
	{
        if($5 == "ac") {$6 = "office_ac"; printf("%-80s\n", $0);}
        else if($5  == "socket") {$6 = "pc_socket"; printf("%-80s\n", $0);}
 	else if($5  == "lighting") {$6 = "office_lighting"; printf("%-80s\n", $0);}
        else { $6 = "null"; printf("%-80s\n", $0);}
}'
