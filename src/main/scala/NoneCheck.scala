/**
  * Created by theone4ever on 2016-10-31.
  */
object NoneCheck {

  class Interest(sliceId: String, nodeId: String){
    def getInterest: String = {



      extractInfo(sliceId)+"."+extractInfo(nodeId)
    }
  }


//  def extract2(s: String) = {
//    Option(s) match {
//      case Full(s) if (!s.isEmpty) => String filled
//      case _ => String was null
//    }
//  }

  def isEmpty(s: String) = {
    Option(s).getOrElse("").isEmpty
  }

  def extractInfo(s: String) = {
    if(isEmpty(s)) "*"
    else s
  }

}
