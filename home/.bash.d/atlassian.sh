JAVA_VERSION=1.7

if [ -x /usr/libexec/java_home ]; then
    export JAVA_HOME=$(/usr/libexec/java_home -v$JAVA_VERSION)
    export ATLAS_HOME=/usr/share/atlassian-plugin-sdk
    export M2_HOME=$ATLAS_HOME/apache-maven

    pathadd $JAVA_HOME/bin
    pathadd $ATLAS_HOME/bin
    pathadd $M2_HOME/bin
fi
