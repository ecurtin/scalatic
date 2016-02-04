package org.framebit.scalatic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import org.joda.time.LocalDateTime

import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scalaj.http.{Http, HttpOptions}

import org.framebit.scalatic.FileIO._
import org.framebit.scalatic.Arguments._
import org.framebit.scalatic.PostSummary._

object Scalatic extends App {
  val GHMDRendererUrl = "https://api.github.com/markdown/raw"

  val options = validateArgs(args).orElse {
    println(
      s"Usage: java -jar scalatic-x.x.x <blogPath> " +
      s"[<source> default 'source'] [<target> default 'target']")
    None
  }

  options.foreach { case (path, source, target) =>
    // TODO OGG: put all these require in create in a method
    // called validBlogFileSystemAnatomy which returns a BlogFileSystemAnatomy object
    val newPath = s"$path/new"
    requireFile(newPath, mustBeFolder = true)

    validateFileNames(newPath)

    val sourcePath = s"$path/$source"
    requireFile(sourcePath, mustBeFolder = true)

    val headerPath = s"$sourcePath/header.html"
    requireFile(headerPath, mustBeFolder = false)

    val footerPath = s"$sourcePath/footer.html"
    requireFile(footerPath, mustBeFolder = false)

    val sourcePostsPath = s"$sourcePath/posts"
    createFolderIfNotExists(sourcePostsPath)

    val targetPath = s"$path/$target"
    createFolderIfNotExists(targetPath)

    val header = stringFromFile(headerPath)
    val footer = stringFromFile(footerPath)
    renderNewPosts(
      newPath, sourcePath, sourcePostsPath, targetPath, header, footer)

    generateIndex(sourcePostsPath, targetPath, header, footer)

    copyFiles(
      sourcePath,
      targetPath,
      excludeFiles = Set("header.html", "footer.html"))
  }


  def generateIndex(
      sourcePostsPath: String,
      targetPath: String,
      header: String,
      footer: String) = {

    val linksToPosts = for (
      srcFile <- Files.newDirectoryStream(Paths.get(sourcePostsPath)).asScala
      if !Files.isDirectory(srcFile)
    ) yield fileNameToPostSummary(srcFile.getFileName.toString)

    val html = linksToPosts.toSeq.sorted.map(toLink).mkString("<br/>\n")
    writeFile(s"$header\n$html\n$footer", s"$targetPath/index.html")
  }

  private def fileNameToPostSummary(fileName: String): PostSummary = {
    val fileNameNoExt = fileName.split('.')(0)
    val url = s"$fileNameNoExt.html"

    val fileNamePiecesNoExt = fileNameNoExt.split("-")
    val fileNameNoDate = fileNamePiecesNoExt.dropRight(5)
    val title = fileNameNoDate.mkString(" ")

    val maxi = fileNamePiecesNoExt.length - 1
    val date = new LocalDateTime(
      fileNamePiecesNoExt(maxi - 4).toInt,
      fileNamePiecesNoExt(maxi - 3).toInt,
      fileNamePiecesNoExt(maxi - 2).toInt,
      fileNamePiecesNoExt(maxi - 1).toInt,
      fileNamePiecesNoExt(maxi).toInt)
    PostSummary(url, title, date)
  }

  def validateFileNames(folderPath: String) = {
    val expected = "Some-blog-post-name-<yyyy-MM-dd-HH-mm>.md"
    val example = "Your-wise-blog-post-name-2015-07-15-00-45.md"
    for (
      file <- Files.newDirectoryStream(Paths.get(folderPath)).asScala if !Files.isDirectory(file);
      fileName <- Option(file.getFileName.toString) if !fileName.startsWith(".")
    ) {
      val fileName = file.getFileName.toString
      val fileNamePieces = fileName.split('.')
      val errMsg =
        s"File name '$fileName' does not match the expected format " +
        s"'$expected' - e.g. '$example'"

      require(fileNamePieces.length == 2, errMsg)
      require(fileNamePieces(0).split("-").length > 5, errMsg)
    }
  }

  private def renderNewPosts(
    newPath: String,
    sourcePath: String,
    sourcePostsPath: String,
    targetPath: String,
    header: String,
    footer: String)
  : Unit = {
    val newPostsFolder = Paths.get(newPath)
    for (
      newSrcFile <- Files.newDirectoryStream(newPostsFolder).asScala
      if !Files.isDirectory(newSrcFile)
    ) {
      val html = render(newSrcFile, header, footer)

      val srcFileName = newSrcFile.getFileName.toString

      val destFileName =
        srcFileName.split('.').dropRight(1).mkString("", ".", ".html")
      writeFile(html, s"$targetPath/$destFileName")

      val processedSrcFilePath = s"$sourcePostsPath/$srcFileName"
      moveFile(newSrcFile, Paths.get(processedSrcFilePath))
    }
  }



  private def render(file: Path, header: String, footer: String): String = {
    val srcFilePath: String = file.toString
    println(s"\nRendering $srcFilePath ...")
    val markdown = stringFromFile(srcFilePath)
    val html = Http(GHMDRendererUrl)
      .postData(markdown)
      .header("Content-Type", "text/plain")
      .header("Charset", "UTF-8")
      .option(HttpOptions.readTimeout(10000))
      .asString.body
    val htmlFull = s"$header\n$html\n$footer"
    htmlFull
  }



}
