export JAVA_HOME=$(/usr/libexec/java_home -v1.8)

#export ATLAS_HOME=$HOME/src/atlassian-plugin-sdk-4.1
export ATLAS_HOME=$HOME/src/atlassian-plugin-sdk-4.2.9
export M2_HOME=$ATLAS_HOME/apache-maven

pathadd $JAVA_HOME/bin
pathadd $ATLAS_HOME/bin
pathadd $M2_HOME/bin
