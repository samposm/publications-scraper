#!/usr/bin/env scala
!#

// The BSD 2-Clause License:
//
// Copyright (c) 2014, Sampo Smolander
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice, this
//   list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

object MonadShit {
  // A monad containing either Good[T] or Error[String]. A bit like
  // the \/ in Scalaz.
  sealed trait Result[+T] {
    def flatMap[U](f: T => Result[U]): Result[U] = this match {
      case Good(x) => f(x)
      case e: Error => e
    }
    def map[U](f: T => U): Result[U] = flatMap { (x: T) => Result(f(x)) }
  }
  case class Good[T](x: T) extends Result[T]
  case class Error(e: String) extends Result[Nothing]
  object Result { def apply[T](x: T): Result[T] = Good(x) }

  // mapM from Haskell, for the Result monad
  // hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:mapM
  def combineWith[T,U](f: T => Result[U])
                      (left: Result[List[U]], right:T): Result[List[U]] =
    left match {
      case e: Error  => e
      case Good(list) => f(right) match {
        case e: Error => e
        case Good(x) => Good(x :: list)
      }
    }
  def mapM[T,U](f: T => Result[U])(list: List[T]): Result[List[U]] = {
    val zeroElem = Result(List[U]())
    val pairCombinator = combineWith(f) _
    list.foldLeft(zeroElem)(pairCombinator).map(_.reverse)
  }
}

object Scraper extends App {

  import MonadShit._

  def readNamesFromFile(fileName: String): Result[List[String]] =
    try {
      val source = io.Source.fromFile(fileName)
      val lines = source.getLines.toList ; source.close()
      return Good(lines.map(_.trim))
    } catch {
      case e: java.io.FileNotFoundException => { return Error(e.toString) }
    }

  // Stupid TUHAT gives http-links, but only works with https-links.
  // (No one notices, because browsers follow the redirects TUHAT gives.)
  def toHttps(url: String): String =
    if (url(4) != 's') { url.patch(4, "s", 0) } else { url }

  def getPage(url: String): String = {
    val url2 = toHttps(url)
    val source = io.Source.fromURL(url2)
      // if the webpage won't answer, will this throw an exception?
    val page = source.mkString ; source.close()
    page
  }

  val searchURLBase =
    "https://tuhat.halvi.helsinki.fi/portal/en/searchall.html?searchall="
  def toSearchURL(s: String) = searchURLBase + s.replace(' ','+')

  val nameMatch = new util.matching.Regex(
    """rel="Person"\s+href="(.+?)".+?class="highlight">""" +
    """(.+?)<.+?class="highlight">(.+?)<""",
    "url", "surname", "firstname"
  )

  def nameToNameURLPair(name: String): Result[(String,String)] = {
    System.err.print(name)
    val searchURL = toSearchURL(name)
    val html1 = getPage(searchURL)
    val i = html1.indexOfSlice("Current employees <span")
    val html2 = html1.drop(i)  // end part of the webpage
    val hits = nameMatch.findAllIn(html2).matchData.toList

    if (hits.isEmpty) {
      System.err.println
      Error(
	"Using the search:\n" + searchURL + "\n" +
	"Cannot find person " + name
      )
    } else if (hits.length > 1) {
      System.err.println
      Error(
	"Using the search:\n" + searchURL + "\n" +
	"Found several people with name " + name + ":\n" +
	hits.map(_.group("url")).map("  " + _).mkString("\n") + "\n" +
	"Please add first middle initial (from their email address) " +
        "for this person"
	)
    } else {
      System.err.println(" ...found")
      Good(name,hits.head.group("url"))
    }
  }

  def URLToPublicationPage1(nameurl:(String, String)): (String,String) = {
    val (name, url) = nameurl // stupid Scala
    (name,url.patch(url.length - 5, "/publications", 0))
  }

  val ampersandMatch = new util.matching.Regex("""amp;""")

  case class Publication(
    link: String, title: String, authors: String, year: Int,
    journal: Option[String], volume: Option[String], number: Option[String],
    pages: Option[String],
    compilationName: Option[String], editors: Option[String],
    publisher: Option[String],compilationPages: Option[String],
    typeShort: String, typeLong: String)
  {
    def toText = {
      val authors2 = ampersandMatch.replaceAllIn(authors,"")
      val journal2 = journal match { case Some(name) => " " + name
				     case None => "" }
      val number2  = number  match { case Some(num) => "(" + num + ")"
				     case None => "" }
      val volume2  = volume  match { case Some(vol) => " " + vol + number2
				     case None => "" }
      val pages2   = pages   match { case Some(pp) => " " + pp + "."
				     case None => "" }

      val compilationName2 = compilationName match {
	case Some(name) => " In: " + name + "."
	case None => ""
      }
      val editors2 = editors match {
	case Some(eds) => {
	  val s = " " + eds + "."
	  ampersandMatch.replaceAllIn(s,"")
	}
	case None => ""
      }
      val publisher2 = publisher match { case Some(pub) => " " + pub + "."
				 	 case None => "" }
      val compilationPages2 = compilationPages match {
	case Some(pp) => if (pages.isDefined) {""} else {" " + pp}
	case None => ""
      }

      authors2 + " (" + year + "): " + title + "." + journal2 +
      volume2 + pages2 + compilationPages2 + compilationName2 + editors2 + 
      publisher2
    }
  }

  val nextPageMatch = new util.matching.Regex(
    """href="(\S+?)"><span>Next</span>""",
    "url"
  )

  val publicationMatch = new util.matching.Regex(
    """<div class="rendering rendering_publication.*?">.*?""" + // -- start
    """<h2 class="title">.*?href="(.+?)">.*?"""               + // link
    """<span>(.+?)</span></a></h2>"""                         + // title
    """(.+?)"""                                               + // authors
    """<span class="date">(.+?)</span>"""                     + // date
    """(.+?)"""                                               + // rest
    """<span class="type_classification">(.+?)</span>"""      + // ptype
    """.*?</div>""",                                            // -- end
    "link", "title", "authors", "date", "rest", "ptype"
  )

  val journalMatch = new util.matching.Regex(
    """<span class="journal">In : <span>(.+?)</span>""",
    "journal"
  )

  val volumeMatch = new util.matching.Regex(
    """<span class="volume">(.+?)</span>""",
    "volume"
  )

  val numberMatch = new util.matching.Regex(
    """<span class="journalnumber">(.+?)</span>""",
    "number"
  )

  val pagesMatch = new util.matching.Regex(
    """<span class="pages">(.+?)</span>""",
    "pages"
  )

  val tagMatch = new util.matching.Regex(
    """<.+?>"""
  )

  val compilationMatch = new util.matching.Regex(
    //"""\s*<em>(.+?)\.</em>\s*(.+?)\.\s*<span>(.+?)</span>""" +
    //"""<span class="numberofpages">(.+?)</span>""",
    """\s*<em>(.+?)\.</em>\s*(.+?)\.\s*<span>(.+?)</span>.+?""" +
    """<span class="numberofpages">(.+?)</span>""",
    "compilationName","editors","publisher","pages"
  )

  // Regex reminder:
  // \s - whitespace
  // \S - nonwhitespace
  //  . - any character
  //  + - 1 or more
  //  * - 0 or more
  //  ? - make non-greedy

  def matchToPublication(m: scala.util.matching.Regex.Match): Publication = {
    val link      = toHttps(m.group("link"))
    val title     = m.group("title")
    val authors1  = m.group("authors")
    val year      = m.group("date").takeRight(4).toInt
    val rest      = m.group("rest")
    val typeShort = m.group("ptype").take(2)
    val typeLong  = m.group("ptype").drop(3)

    // Option values:
    val journal = journalMatch.findFirstMatchIn(rest).map(_.group("journal"))
    val volume  = volumeMatch.findFirstMatchIn(rest).map(_.group("volume"))
    val number  = numberMatch.findFirstMatchIn(rest).map(_.group("number"))
    val pages   = pagesMatch.findFirstMatchIn(rest).map(_.group("pages"))

    val compilationInfo = compilationMatch.findFirstMatchIn(rest)
    val compilationName = compilationInfo.map(_.group("compilationName"))
    val editors = compilationInfo.map(_.group("editors"))
    val publisher = compilationInfo.map(_.group("publisher"))
    val compilationPages = compilationInfo.map(_.group("pages"))

    val authors2 = tagMatch.replaceAllIn(authors1,"").trim

    Publication(link, title, authors2, year, journal, volume, number, pages,
		compilationName,editors,publisher,compilationPages,
		typeShort, typeLong)
  }

  def helper(endYear: Int, url: String, pagecount: Int, acc: List[Publication]):
  List[Publication] = {
    val html = getPage(url)
    val nextPageHits = nextPageMatch.findAllIn(html).matchData.toList

    val pubHits = publicationMatch.findAllIn(html).matchData
    val (good,rest) =
      pubHits.map(matchToPublication).span(_.year >= endYear)
    val publications = good.toList.reverse

    System.err.print(" page" + pagecount)

    if (!rest.isEmpty) { return publications ::: acc }
    if (nextPageHits.isEmpty) {
      return publications ::: acc
    } else if (nextPageHits.length > 1) {
      System.err.println("\nError: something wrong in function: helper")
      sys.exit(1)
    } else {
      helper(endYear, nextPageHits.head.group("url"), pagecount + 1,
	     publications ::: acc)
    }
  }

  def page1toPublications(endYear: Int)(nameurl: (String,String)):
  List[Publication] = {
    val (name, url) = nameurl
    System.err.print(name + "'s publications:")
    val result = helper(endYear, url, 1, List()).reverse
    System.err.println
    result
  }

  // A1 Refereed journal article
  // A3 Contribution to book/other compilations (refereed)
  // B2 Contribution to book/other compilations (non-refereed)
  def isGoodPub(p: Publication): Boolean =
    List("A1","A3","B2").contains(p.typeShort)

  def pubOrder(p1: Publication, p2: Publication): Boolean =
    if (p1.year != p2.year) {
      p1.year > p2.year
    } else {
      // Hack
      val initialsMatch = new util.matching.Regex("""\s\W*?\w?(\W\w)?\W*?\s""")
      val surnames1 = initialsMatch.replaceAllIn(p1.authors," ")
      val surnames1b = initialsMatch.replaceAllIn(surnames1," ")
      val surnames2 = initialsMatch.replaceAllIn(p2.authors," ")
      val surnames2b = initialsMatch.replaceAllIn(surnames2," ")
      val alist1 = surnames1b.split(" ") // alist = author list
      val alist2 = surnames2b.split(" ")
      if (alist1(0) != alist2(0)) {
	alist1(0) < alist2(0)
      } else if (alist1(1) != alist2(1)) {
	alist1(1) < alist2(1)
      } else {
	p1.title < p2.title	
      }
    }

  // main ************************************************************

  if (args.length != 2) {
    System.err.println("Usage: tuhat-spider startyear <namefile>") ; sys.exit(1)
  }
  val endYear = args(0).toInt
  val inputFile = args(1)

  // Monadic for-comprehension with error handling:
  val result1 = for (
    names          <- readNamesFromFile(inputFile) ;
    NameURLPairs   <- mapM(nameToNameURLPair)(names)
  ) yield NameURLPairs
  // names: List[String], names of people
  // NameURLPairs: List[(String,String)], list of (name,searchurl) pairs
  // result1: Result[List[(String,String)]]
  val result2 = result1 match {
    case Error(e) => { System.err.println("Error:\n" + e) ; sys.exit(1) }
    case Good(x) => x
  }
  // result2: List[(String,String)]

  // No more error handling:
  val result3 = for (
    nameURLPair <- result2 ;
    page1       =  URLToPublicationPage1(nameURLPair) ;
                   // flatMap happens↴
    pub         <- page1toPublications(endYear)(page1) if isGoodPub(pub)
  ) yield pub
  // result3: List[Publication], undesired publication types removed

  val result4 = result3.distinct.sortWith(pubOrder)

  println(result4.map(_.toText).mkString("\n\n"))

}

Scraper.main(args)

