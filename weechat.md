# Setting Up WeeChat



``` shell
/set buflist.format.buffer "${if:${type}==server?${color:black,31}${format_number}${color:white}:${color:239}${format_number}${indent}${color_hotlist}}${if:${type}!=0&&${type}!=exec?${name}:${name}}${format_hotlist}${if:${buffer.full_name}==perl.iset? ${color:31}${buffer.local_variables.iset_filter}} ${color:31}${buffer.local_variables.buflist}"

```
