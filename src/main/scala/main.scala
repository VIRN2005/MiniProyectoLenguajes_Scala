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
    printMatrix(DenseMatrix(matrix: _*))

    // Paso 1: Centrar y reducir la matriz original de datos
    println("\n>> MATRIZ ESTANDARIZADA <<")
    val breezeMatrix = DenseMatrix(matrix: _*)
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
    val qualityIndividuals = DenseVector(principalComponents(*, ::).map(row => math.pow(norm(row), 2)).toArray)
    println(qualityIndividuals.toArray.map(el => f"[$el%8.4f  ]").mkString(" "))

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
