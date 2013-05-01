export JAVA_HOME=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home

export ATLAS_HOME=$HOME/src/atlassian-plugin-sdk-4.1
pathadd $ATLAS_HOME/bin
pathadd $ATLAS_HOME/apache-maven/bin
alias mvn=atlas-mvn
