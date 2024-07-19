import cats.effect.kernel.{Resource, Sync}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.given

import javax.crypto.{Cipher, CipherOutputStream}
import java.io.FileInputStream
import java.io.FileOutputStream
import javax.crypto.spec.SecretKeySpec

enum Mode(val value : Int) {
  case Encrypt extends Mode(Cipher.ENCRYPT_MODE)
  case Decrypt extends Mode(Cipher.DECRYPT_MODE)
}

object catsMain extends IOApp {
  val key = "hello hussain"
  private def blowFish(key : String, mode : Mode) = {
    val secretKey = new SecretKeySpec(key.getBytes(), "Blowfish")
    val cipher: Cipher = Cipher.getInstance("Blowfish")
    cipher.init(mode.value, secretKey)
    cipher
  }

  override def run(args : List[String]): IO[ExitCode] = {
    val srcPath = "ccc.txt"
    val dstPath = "rrr.txt"
    val key = "encryption key"
    val op = Mode.Encrypt
    for {
      _ <- IO.println("Starting...")
      _ <- IO.raiseWhen(srcPath == dstPath)(IllegalArgumentException("Source and Destination paths cannot be the same"))
      _ <- encryptDecrypt[IO](srcPath, dstPath, blowFish(key, Mode.Decrypt))
      _ <- IO.println("Finished...")
    } yield ExitCode.Success
  }

  private def encryptDecrypt[F[_] : Sync](src : String, dst : String, cipher : Cipher): F[Long] = files2Streams[F](src, dst, cipher).use{
    case (inS, outS) =>
      val buf = Array.ofDim[Byte](64)

      var read = inS.read(buf)
      while (read != -1) {
        outS.write(buf, 0, read)
        read = inS.read(buf)
      }
      Sync[F].pure(1)
  }

  private def files2Streams[F[_] : Sync](src : String, dst : String, cipher : Cipher) : Resource[F, (FileInputStream, CipherOutputStream)] = for{
    inStrm <- Resource.make { Sync[F].blocking(FileInputStream(src)) } { inStream =>
      Sync[F].blocking(inStream.close()).handleErrorWith(_ => Sync[F].unit) }
    otStrm <- Resource.make { Sync[F].blocking(FileOutputStream(dst)) } { otStream =>
      Sync[F].blocking(otStream.close()).handleErrorWith(_ => Sync[F].unit) }
    cpStrm <- Resource.make { Sync[F].blocking(CipherOutputStream(otStrm, cipher)) } { cpStream =>
      Sync[F].blocking(cpStream.close()).handleErrorWith(_ => Sync[F].unit) }
  }yield(inStrm, cpStrm)

}
