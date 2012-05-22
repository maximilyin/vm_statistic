#!/bin/sh
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -sname statistic -s vm_statistic  
 
    
