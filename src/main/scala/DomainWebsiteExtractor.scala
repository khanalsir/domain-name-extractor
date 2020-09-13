object DomainWebsiteExtractor extends App {

  //Website Prefixes List
  val schemesList = List(
    /*This should be in first of other schemes "https://www.", "http://www.", "mailto:www.", "news:www.", "www."*/
    "https://", "http://", "mailto:", "news:"
  )


  /**
    * This Method returns only Domain of Website
    * @param fullWebsite
    * @return Option[String]
    */
  def onlyDomainWebsite(fullWebsite: Option[String]): Option[String] = {
    fullWebsite match {
      case Some(website) => {
        val prefixProtocol = schemesList.find(k => k.contains(website.split(k).head.trim()))
        if(prefixProtocol.isDefined){
          val onlyDomain = website.split(prefixProtocol.get).last.trim()
          Some(onlyDomain)
        } else Some(website)
      }
      case None => None
    }
  }

  /**
    * This Method returns only Scheme/Protocol of Website
    * @param fullWebsite
    * @return Option[String]
    */
  def onlyWebsiteScheme(fullWebsite: Option[String]): Option[String] = {
    fullWebsite match {
      case Some(website) => {
        schemesList.find(k => k.contains(website.split(k).head.trim()))
      }
      case None => None
    }
  }

  println("*****************Only Domain Website***********************")
  println(onlyDomainWebsite(Some("https://www.facebook.com"))) // Returns www.facebook.com

  println("*****************Only Website Scheme***********************")
  println(onlyDomainWebsite(Some("https://www.facebook.com"))) // Returns https://


}
