package wildcat.assembler

import wildcat.assembler.compiler.WorkflowCompiler

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.sys.process._

class AssemblerTest(testFolderPath: String) {
  private val path: Path = Paths.get(testFolderPath)
  print(f"Testing files in: $path\n")
  if (Files.exists(path) && Files.isDirectory(path)) {
    val stream = Files.list(path) // Stream[Path]
    try {
      stream.iterator().asScala
        .filter { file =>
          val fileName = file.getFileName.toString.toLowerCase
          fileName.endsWith(".s") || fileName.endsWith(".asm")
        }
        .foreach { file =>
          print(f"Testing $file\n")
          val asm = Files.readAllLines(file).asScala.toArray
          print(asm.mkString("\n")+"\n")
          val assembly = Source.fromFile(file.toString).mkString
          val myBin = WorkflowCompiler(assembly).right.get.map(int => f"$int%08x")

          val tmpFile = s"$path/tmp_${file.getFileName}.o"
          val cmd = Seq("riscv64-unknown-elf-as", file.toString, "-o", tmpFile) #|
            Seq("riscv64-unknown-elf-objdump", "-d", tmpFile) #|
            Seq("grep", "^ ") #|
            Seq("awk", "{print $2}")
          val logger = ProcessLogger(_ => (), _ => ())
          val refHex = cmd.!!(logger).split("\n")
          //val refBin = refHex.map(hex => BigInt(hex, 16).toLong)

          for ((asm, my, ref) <- asm.lazyZip(myBin).lazyZip(refHex).toArray) {
            if (my != ref) {
              print(f"Failed test file: $file\n")
              print(  "Instruction        myHex                   refHex\n")
              print(f"❌ $asm           ${my}               ${ref}\n")
              print(f"                             ${Integer.parseInt(my, 16).toBinaryString.reverse.padTo(32, '0').reverse}               ${Integer.parseInt(ref, 16).toBinaryString.reverse.padTo(32, '0').reverse}\n")
              throw new AssertionError(s"Mismatch in $file: $asm")
            }
          }
          print(f"✅ $file Succeeded\n\n")
        }
    } finally {
      stream.close()
    }
  }

  def toBin(n: Long): String = {
    n.toInt.toBinaryString.reverse.padTo(32, '0').reverse
  }

  def toHex(n: Long): String = {
    f"$n%08x"
  }

  def arrayToHex(array: Array[Long]): Unit = {
    array.foreach(i =>
      print(f"$i%08x\n"))
  }
}