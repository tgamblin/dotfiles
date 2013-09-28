if [ -n "$MACPORTS_HOME" ]; then
    alias mysqlstart="sudo $MACPORTS_HOME/bin/mysqld_safe5"
    alias mysqlstop="$MACPORTS_HOME/bin/mysqladmin5 -u root -p shutdown"
fi
