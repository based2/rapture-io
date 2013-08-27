name := "rapture-io"

version := "0.8"

organization := "com.propensive"

scalaVersion := "2.10.2"

resolvers ++= Seq(
 "Local" at "file:///W:/repository",
 "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
 "Maven2" at "http://repo1.maven.org/maven2"
)

libraryDependencies ++= Seq(
    "javax.mail"     % "mail"   % "1.4.7"   % "Compile"
)

// http://groups.google.com/group/simple-build-tool/browse_thread/thread/75a3d90e382a8b94
//unmanagedBase <<= baseDirectory { base => base / "lib" }
// unmanagedJars in Compile <<= baseDirectory { base => (base / "lib" / "a.jar").get }

