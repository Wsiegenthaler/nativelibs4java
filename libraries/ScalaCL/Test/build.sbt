//name := "scalacl-test"

//version := "0.3-SNAPSHOT"

//organization := "com.nativelibs4java"

//scalaVersion := "2.9.1"

resolvers ++= Seq(
        "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository",
        "NativeLibs4Java Repository" at "http://nativelibs4java.sourceforge.net/maven/"
)

libraryDependencies ++= Seq(
        "com.nativelibs4java" % "scalacl" % "0.3-SNAPSHOT" classifier "shaded",
        "com.novocode" % "junit-interface" % "0.8" % "test->default"
)


autoCompilerPlugins := true

addCompilerPlugin("com.nativelibs4java" % "scalacl-compiler-plugin" % "0.3-SNAPSHOT")

