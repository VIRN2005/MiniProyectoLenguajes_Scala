file:///C:/Users/DELL/Documents/ACP_SCALA/src/main/scala/main.scala
### java.lang.NoClassDefFoundError: sourcecode/Name

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 3338
uri: file:///C:/Users/DELL/Documents/ACP_SCALA/src/main/scala/main.scala
text:
```scala
import com.opencsv.CSVReaderBuilder
import com.opencsv.CSVParserBuilder
import java.io.FileReader
import scala.jdk.CollectionConverters._
import breeze.linalg._
import breeze.stats._

object MainApp {
  def readAndProcessCSV(filePath: String): Array[Array[Double]] = {
    val parser = new CSVParserBuilder()
      .withSeparator(';')
      .build()
    val reader = new CSVReaderBuilder(new FileReader(filePath))
      .withCSVParser(parser)
      .build()
    try {
      val allRows = reader.readAll().asScala.toList
      val matrix = allRows.tail.map { row =>
        row.tail.map { value =>
          value.replace(',', '.').toDouble
        }.toArray
      }.toArray
      matrix
    } finally {
      reader.close()
    }
  }

  def printMatrix(matrix: DenseMatrix[Double]): Unit = {
    matrix(*, ::).foreach { row =>
      println(row.toArray.map(el => f"[$el%8.4f  ]").mkString(" "))
    }
  }

  def printMatrix(array: Array[Array[Double]]): Unit = {
    array.foreach { row =>
      println(row.map(el => f"[$el%8.4f  ]").mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    // Leer el archivo CSV
    val filePath = System.getProperty("user.dir") + "/EjemploEstudiantes.csv"
    val matrix = readAndProcessCSV(filePath)
    println(">> MATRIZ <<")
    printMatrix(DenseMatrix(matrix*))

    // Paso 1: Centrar y reducir la matriz original de datos
    println("\n>> MATRIZ ESTANDARIZADA <<")
    val breezeMatrix = DenseMatrix(matrix*)
    val meanVector = mean(breezeMatrix(::, *))
    val stddevVector = stddev(breezeMatrix(::, *))
    val standardizedMatrix = DenseMatrix.zeros[Double](breezeMatrix.rows, breezeMatrix.cols)

    for (col <- 0 until breezeMatrix.cols) {
      val colData = breezeMatrix(::, col)
      val standardizedCol = (colData - meanVector(col)) / stddevVector(col)
      standardizedMatrix(::, col) := standardizedCol
    }
    printMatrix(standardizedMatrix)

    // Paso 2: Calcular la matriz de correlaciones
    println("\n>> MATRIZ DE CORRELACIONES <<")
    val correlationMatrix = cov(standardizedMatrix)
    printMatrix(correlationMatrix)

    // Paso 3: Calcular y ordenar los valores y vectores propios de la matriz de correlaciones
    val eigResult = eig(correlationMatrix)

    val eigenvalues = eigResult.eigenvalues
    val eigenvectors = eigResult.eigenvectors

    val sortedIndices = eigenvalues.toArray.zipWithIndex.sortBy(-_._1).map(_._2)
    val sortedEigenvalues = DenseVector(sortedIndices.map(i => eigenvalues(i)))
    val sortedEigenvectors = DenseMatrix.tabulate(eigenvectors.rows, eigenvectors.cols) { (i, j) =>
      eigenvectors(i, sortedIndices(j))
    }

    println("\n>> EIGENVALORES ORDENADOS [MAYOR A MENOR] <<")
    println(sortedEigenvalues.toArray.map(el => f"[$el%8.4f  ]").mkString(" "))

    println("\n>> EIGENVECTORES ORDENADOS [MAYOR A MENOR] <<")
    printMatrix(sortedEigenvectors)

    // Paso 5: Calcular la matriz de componentes principales
    println("\n>> MATRIZ DE COMPONENTES PRINCIPALES <<")
    val principalComponents = standardizedMatrix * sortedEigenvectors
    printMatrix(principalComponents)

    // Paso 6: Calcular la matriz de calidades de individuos
    println("\n>> MATRIZ DE CALIDADES DE INDIVIDUOS <<")
    for (rows <_@@)

    // Paso 7: Calcular la matriz de coordenadas de las variables
    println("\n>> MATRIZ DE COORDENADAS DE LAS VARIABLES <<")
    val variableCoordinates = sortedEigenvectors.t * standardizedMatrix.t
    printMatrix(variableCoordinates)

    // Paso 8: Calcular la matriz de calidades de las variables
    println("\n>> MATRIZ DE CALIDADES DE LAS VARIABLES <<")
    val qualityVariables = DenseVector(variableCoordinates(*, ::).map(col => math.pow(norm(col), 2)).toArray)
    println(qualityVariables.toArray.map(el => f"[$el%8.4f  ]").mkString(" "))

    // Paso 9: Calcular el vector de inercias de los ejes
    println("\n>> VECTOR DE INERCIAS DE LOS EJES <<")
    val inertiaVector = DenseVector(sortedEigenvalues.map(el => math.pow(el, 2)).toArray)
    println(inertiaVector.toArray.map(el => f"[$el%8.4f  ]").mkString(" "))

    // Paso 10: Graficar
    //val f = Figure()
    //val p = f.subplot(0)
    //p += plot(inertiaVector.toArray, '+')
    //p.xlabel = "Ejes"
    //p.ylabel = "Inercia"
    //f.saveas("inercia_ejes.png")
  }
}

```



#### Error stacktrace:

```
scala.meta.internal.tokenizers.XmlParser$Xml$.UnpStart(XmlParser.scala:44)
	scala.meta.internal.tokenizers.XmlParser$Xml$.Unparsed(XmlParser.scala:43)
	scala.meta.internal.tokenizers.XmlParser$Xml$.XmlContent(XmlParser.scala:39)
	scala.meta.internal.tokenizers.XmlParser.$anonfun$XmlExpr$1(XmlParser.scala:25)
	scala.meta.shaded.internal.fastparse.internal.RepImpls$.rec$4(RepImpls.scala:226)
	scala.meta.shaded.internal.fastparse.internal.RepImpls$.rep$extension(RepImpls.scala:266)
	scala.meta.shaded.internal.fastparse.package$ByNameOps$.rep$extension(package.scala:202)
	scala.meta.internal.tokenizers.XmlParser.XmlExpr(XmlParser.scala:25)
	scala.meta.internal.tokenizers.LegacyScanner.$anonfun$getXml$2(LegacyScanner.scala:823)
	scala.meta.shaded.internal.fastparse.SharedPackageDefs.parseInputRaw(SharedPackageDefs.scala:69)
	scala.meta.shaded.internal.fastparse.SharedPackageDefs.parseInputRaw$(SharedPackageDefs.scala:45)
	scala.meta.shaded.internal.fastparse.package$.parseInputRaw(package.scala:6)
	scala.meta.shaded.internal.fastparse.Parsed$Extra.trace(Parsed.scala:139)
	scala.meta.internal.tokenizers.LegacyScanner.getXml(LegacyScanner.scala:826)
	scala.meta.internal.tokenizers.LegacyScanner.fetchLT$1(LegacyScanner.scala:300)
	scala.meta.internal.tokenizers.LegacyScanner.fetchToken(LegacyScanner.scala:307)
	scala.meta.internal.tokenizers.LegacyScanner.scala$meta$internal$tokenizers$LegacyScanner$$nextToken(LegacyScanner.scala:195)
	scala.meta.internal.tokenizers.LegacyScanner.nextToken(LegacyScanner.scala:172)
	scala.meta.internal.tokenizers.ScalametaTokenizer.nextToken$1(ScalametaTokenizer.scala:28)
	scala.meta.internal.tokenizers.ScalametaTokenizer.emitTokenWhitespace$1(ScalametaTokenizer.scala:64)
	scala.meta.internal.tokenizers.ScalametaTokenizer.emitToken$1(ScalametaTokenizer.scala:143)
	scala.meta.internal.tokenizers.ScalametaTokenizer.loop$1(ScalametaTokenizer.scala:152)
	scala.meta.internal.tokenizers.ScalametaTokenizer.uncachedTokenize(ScalametaTokenizer.scala:162)
	scala.meta.internal.tokenizers.ScalametaTokenizer.$anonfun$tokenize$1(ScalametaTokenizer.scala:16)
	scala.collection.concurrent.TrieMap.getOrElseUpdate(TrieMap.scala:962)
	scala.meta.internal.tokenizers.ScalametaTokenizer.tokenize(ScalametaTokenizer.scala:16)
	scala.meta.internal.tokenizers.ScalametaTokenizer$$anon$1.apply(ScalametaTokenizer.scala:313)
	scala.meta.tokenizers.Api$XtensionTokenizeDialectInput.tokenize(Api.scala:22)
	scala.meta.tokenizers.Api$XtensionTokenizeInputLike.tokenize(Api.scala:13)
	scala.meta.internal.mtags.ScalametaCommonEnrichments$XtensionStringDocMeta.safeTokenize(ScalametaCommonEnrichments.scala:237)
	scala.meta.internal.pc.completions.KeywordsCompletions$.reverseTokens$lzyINIT1$1(KeywordsCompletions.scala:49)
	scala.meta.internal.pc.completions.KeywordsCompletions$.reverseTokens$1(KeywordsCompletions.scala:53)
	scala.meta.internal.pc.completions.KeywordsCompletions$.contribute(KeywordsCompletions.scala:55)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:188)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:89)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:155)
```
#### Short summary: 

java.lang.NoClassDefFoundError: sourcecode/Name