## Oracle settings
export ORACLE_HOME=/usr/lib/oracle/12.1/client64
export ORACLE_HOSTNAME=oracle12c01.localdomain
export ORACLE_UNQNAME=cdb1
export ORACLE_SID=cdb1
# necessary for sqlplus to start
export LD_LIBRARY_PATH=/usr/lib/oracle/12.1/client64/lib/:$LD_LIBRARY_PATH
export PATH=$PATH:$ORACLE_HOME/bin
