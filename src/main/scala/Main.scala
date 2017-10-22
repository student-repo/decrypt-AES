
import sys.process._

object Main {
  def main(args: Array[String]): Unit = {

    val key = "3e40e632793f1147f0cb59688b757cfee69bb2a93d80a6013ef472fee65482f7"
    val iv = "d79aa8a2fcdb3d1a362a88c5676b04ca"
    val cryptogram = "4h14Iwc23yhjzyZ6Eu65odlMXjNwNY5IN1BYeFu+ioQSKdf4i7Zo+FJhwTWg/nHPV6DhP7szms+pcgiHeYttveEFYA9zwwdeiLjaj22HVZHPEycXcFLl6mXNMzEx+wRSkylDRJF3zgdFzNyj4aSxZg==%"
    val alphabet = ('0' to '9').toSet ++ ('a' to 'f')

    crackPassword(4 ,alphabet, "e632793f1147f0cb59688b757cfee69bb2a93d80a6013ef472fee65482f7", iv, cryptogram)
  }

  def decryptMsg(k: String, iv:String, c:String):Option[String] =
    try {
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      Some(s"""echo -e $c""" #> s"""openssl enc -A -aes-256-cbc -base64 -d -K $k -iv $iv""" !! ProcessLogger(stdout append _, stderr append _))
    } catch {
      case _ => None
    }

  def encryptMsg(msg: String, iv:String, k: String):Option[String] =
    try {
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      Some(s"""echo -e $msg""" #> s"""openssl enc -A -aes-256-cbc -base64 -K $k -iv $iv""" !! ProcessLogger(stdout append _, stderr append _))
    } catch {
      case _ => None
    }

  def getAllPassPossibility(alphabet:Set[Char], length:Int):Set[String] =
    (alphabet.mkString("") * length).combinations(length).flatMap(_.toString.permutations).toSet

  def crackPassword(passPrefixLength:Int, alphabet:Set[Char], keySuffix: String, iv:String, cryptogram:String) = {
    import scala.collection.parallel._

    val passPossibilities = getAllPassPossibility(alphabet, passPrefixLength).par
    passPossibilities.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(8))
    passPossibilities.foreach(p => {
              val key = p + keySuffix
              val a = ('a' to 'z').toSet ++ ('A' to 'Z')
              decryptMsg(key, iv, cryptogram) match {
                case Some(msg) => if(containsNoSpecialChars(msg)) println("Found key: " + key)
                case None => print("")
              }
            })
    println("Finished")
  }
  def containsNoSpecialChars(string: String) = string.matches("^[a-zA-Z0-9\n\t\b .,_\\-.:;\\?!\\/'\"\\*ąćęłńóśźżĄĆĘŁŃÓŚŻŹ\\[\\]\\(\\)]*$")
}