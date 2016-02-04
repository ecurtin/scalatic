package org.framebit.scalatic

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, StandardCopyOption, Files, Paths}
import scala.collection.JavaConverters._


object FileIO {

  def createFolderIfNotExists(pathToFolder: String) = {
    val folder = Paths.get(pathToFolder)
    if (Files.notExists(folder) || !Files.isDirectory(folder))
      Files.createDirectory(folder)
  }

  def stringFromFile(filePath: String): String = {
    val source = io.Source.fromFile(filePath)
    try source.getLines mkString "\n" finally source.close()
  }

  def copyFiles(
                         srcFolderPath: String,
                         destFolderPath: String,
                         excludeFiles: Set[String]): Unit = {
    val srcFolder = Paths.get(srcFolderPath)
    for (
      file <- Files.newDirectoryStream(srcFolder).asScala
      if !excludeFiles(file.getFileName.toString) && !Files.isDirectory(file)
    ) {
      val destFile = Paths.get(s"$destFolderPath/${file.getFileName.toString}")
      println(s"Copying ${file.toString} to ${destFile.toString} ...")
      Files.copy(
        file,
        destFile,
        StandardCopyOption.REPLACE_EXISTING)
    }
  }


  def requireFile(pathToFile: String, mustBeFolder: Boolean) = {
    val requiredFile = Paths.get(pathToFile)
    val isFolder = Files.isDirectory(requiredFile)
    val folderOrFile = if (mustBeFolder) "folder" else "file"
    require(
      Files.exists(requiredFile) && (if (mustBeFolder) isFolder else !isFolder),
      s"$pathToFile does not exist or is not a $folderOrFile")
  }

  def moveFile(srcFile: Path, destFile: Path) = {
    println(s"Moving ${srcFile.toString } to ${destFile.toString} ...")
    Files.move(srcFile, destFile, StandardCopyOption.REPLACE_EXISTING)
  }

  def writeFile(fileContent: String, filePath: String): Path = {
    println(s"Writing $filePath ...")
    Files.write(
      Paths.get(filePath),
      fileContent.getBytes(StandardCharsets.UTF_8))
  }

}
