import akka.actor._
import scala.util.parsing.combinator._

import java.io._

import org.opencv.core.Core
import org.opencv.core.Mat
import org.opencv.core.CvType
import org.opencv.highgui.Highgui
import org.opencv.highgui.VideoCapture

import twitter4j._

case class Tweet(text:String)
case class MediaTweet(text:String,path:String)
case class ReplyTweet(text:String,screenName:String, id:Long)
class TwitterActor extends Actor {
  val t = TwitterFactory.getSingleton
  def receive = {
	case MediaTweet(text,path) => {
		t.updateStatus(
			new StatusUpdate(text)
			.media(new File(path)))
	}
	case ReplyTweet(text,sc,id) => {
		t.updateStatus(
			new StatusUpdate(s"@${sc} ${text}")
			.inReplyToStatusId(id))
	}
	case Tweet(text) => t.updateStatus(text)
  }
}

object MediaCapture {
  def capture(id:Int = 0,path:String) = {
	val cam = new VideoCapture(id)
	val mat = new Mat()
	cam.read(mat)

	Highgui.imwrite(path,mat)
	
	mat.release
	cam.release
  }
}

case class TweetParam(key:String, value:String)
case class TweetParams(owner:String, params:List[TweetParam])
class TweetParser(val screenName : String) extends RegexParsers {
	def chars = "[a-zA-Z0-9_]+".r
	def mention = s"@${screenName}"
	def instruction = (chars) ~ ("=".? ~> chars).?  ^^ {
		case key ~ Some(value) => TweetParam(key,value) 
		case key ~ None => TweetParam(key,"")
	}
	def tweet = mention ~> rep(instruction)

	def parseTweet(s:String) = parseAll(tweet,s) match {
		case Success(result, _) => Some(result)
		case failure:NoSuccess => None
	}
	def parseStatus(s:Status) : TweetParams = parseTweet(s.getText) match {
		case Some(params) => TweetParams(s.getUser.getScreenName,params)
		case None => TweetParams(s.getUser.getScreenName,List.empty[TweetParam])
	}
}

object Main extends StatusListener {
  val parser = new TweetParser("0xb9")
  /* Option Parameters */
  val system = ActorSystem("MySystem")
  val tweet = system.actorOf(Props[TwitterActor],name = "Tweet")

  var adminScreenName = "kamiya_owl"
  var cameraId = 1
  var savePath = "tmp.jpg"
  var shotEnable = true
  var shotDuration = 30//sec
  
  def shot(text:String) = {
  	println("shot started")
  	try {
		if(shotEnable) {
			MediaCapture.capture(cameraId,savePath)
			tweet ! MediaTweet(text,savePath)
		}
	} catch {
		case e:Exception => println(s"D ${adminScreenName} ${e}")
	}
	println("shot finished")
  }
  def onStatus(s:Status) = {
  	println(s.getUser.getId)
  	println(s"@${s.getUser.getScreenName} : ${s.getText}")
	val result = parser.parseStatus(s)
	println(result)
	//
	if(result.owner == adminScreenName)
		result.params.foreach(param => param.key match {
			case "cameraId" => {
				cameraId = param.value.toInt
				tweet ! ReplyTweet(s"Accept. cameraId = ${cameraId}",result.owner,s.getId)
			}
			case "savePath" => {
				savePath = param.value
				tweet ! ReplyTweet(s"Accept. savePath = ${savePath}",result.owner,s.getId)
			}
			case "shotEnable" => {
				shotEnable = param.value.toBoolean
				tweet ! ReplyTweet(s"Accept. shotEnable = ${shotEnable}",result.owner,s.getId)
			}
			case "shotDuration" => {
				shotDuration = param.value.toInt
				tweet ! ReplyTweet(s"Accept. shotDuration = ${shotDuration}",result.owner,s.getId)
			}
			case _ => Unit
		})
	//normal
	result.params.foreach(param => param.key match {
		case "ping" => tweet ! ReplyTweet(s"pong ${param.value}",result.owner,s.getId)
		case "shot" => shot(s"shot command from @${result.owner}")
		case _ => Unit
	})
	println("command parsed")
  }
  def onDeletionNotice(sdn:StatusDeletionNotice) = {}
  def onTrackLimitationNotice(i:Int) = {}
  def onScrubGeo(lat:Long, lng:Long) = {}
  def onException(ex:Exception) = {}
  def onStallWarning(warn:StallWarning) = {}

  def main(args:Array[String]) {
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
	/* config */
	val config = new java.util.Properties
	config.load(new java.io.FileInputStream("rasshot.conf"))
  	adminScreenName = config.getProperty("adminScreenName")
	cameraId = config.getProperty("cameraId").toInt
	savePath = config.getProperty("savePath")
	shotEnable = config.getProperty("shotEnable").toBoolean
	shotDuration = config.getProperty("shotDuration").toInt
	
	/* twitter stream */
	val ts = TwitterStreamFactory.getSingleton
	ts.addListener(this)
	ts.user//start stream
	/* update loop */
	var lastShot = System.currentTimeMillis
	while(true) {
		if(System.currentTimeMillis - lastShot > (shotDuration * 1000)){
			shot(s"${shotDuration} sec interval update")
			lastShot = System.currentTimeMillis
		}
	}
  }
}
