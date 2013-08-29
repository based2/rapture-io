name := "rapture-io"

version := "0.8"

organization := "com.propensive"

scalaVersion := "2.10.2"

resolvers ++= Seq(
 "Local" at "./",
 "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
 "Maven2" at "http://repo1.maven.org/maven2"
)

libraryDependencies ++= Seq(
    "javax.mail"     % "mail"   % "1.4.7"   % "Compile"
)


