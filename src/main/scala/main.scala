import com.opencsv.CSVReaderBuilder
import com.opencsv.CSVParserBuilder
import java.io.FileReader
import scala.jdk.CollectionConverters._
import breeze.linalg._
import breeze.stats._
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.data.xy._
import org.jfree.chart.annotations._

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

  def graphicPlaneMain(pc1: Array[Double], pc2: Array[Double], labels: Array[String]): Unit = {
    val dataset = new XYSeriesCollection()
    val series = new XYSeries("Puntos")
    for (i <- pc1.indices) {
      series.add(pc1(i), pc2(i))
    }
    dataset.addSeries(series)

    val chart = ChartFactory.createScatterPlot(
      "Análisis de componentes principales",
      "Componente Principal 1",
      "Componente Principal 2",
      dataset
    )

    val plot = chart.getXYPlot
    for (i <- pc1.indices) {
      val annotation = new XYTextAnnotation(labels(i), pc1(i), pc2(i))
      plot.addAnnotation(annotation)
    }

    val frame = new ChartFrame("Gráfico", chart)
    frame.pack()
    frame.setVisible(true)
  }
  
  def main(args: Array[String]): Unit = {
    // Leer el archivo CSV
    val filePath = System.getProperty("user.dir") + "/EjemploEstudiantes.csv"
    val matrix = readAndProcessCSV(filePath)
    println(">> MATRIZ <<")
    printMatrix(DenseMatrix(matrix*))

    // Paso 1: Centrar y reducir la matriz original de datos
    println("\n1. >> MATRIZ ESTANDARIZADA <<")
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
    println("\n2. >> MATRIZ DE CORRELACIONES <<")
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

    println("\n3. >> EIGENVALORES ORDENADOS [MAYOR A MENOR] <<")
    println(sortedEigenvalues.toArray.map(el => f"[$el%8.4f  ]").mkString(" "))

    println("\n   >> EIGENVECTORES ORDENADOS [MAYOR A MENOR] <<")
    printMatrix(sortedEigenvectors)

    // Paso 5: Calcular la matriz de componentes principales
    println("\n5. >> MATRIZ DE COMPONENTES PRINCIPALES <<")
    val principalComponents = standardizedMatrix * sortedEigenvectors
    printMatrix(principalComponents)

    // Paso 6: Calcular la matriz de calidades de individuos
    println("\n6. >> MATRIZ DE CALIDADES DE INDIVIDUOS <<")
    val matrixComponentsSquared = principalComponents.map(x => x * x)
    val matrixStandardizedSquared = standardizedMatrix.map(x => x * x)
    val matrixQualityInvdividuals = DenseMatrix.zeros[Double](breezeMatrix.rows, breezeMatrix.cols)
    for (i <- 0 until breezeMatrix.rows) {
      val sumComponents = sum(matrixStandardizedSquared(i, ::))
      for (r <- 0 until breezeMatrix.cols) {
        matrixQualityInvdividuals(i, r) = matrixComponentsSquared(i, r) / sumComponents
      }
    }
    printMatrix(matrixQualityInvdividuals)

    // Paso 7: Calcular la matriz de coordenadas de las variables (mxm) ---------------------------------------------------------------------
    println("\n7. >> MATRIZ DE COORDENADAS DE LAS VARIABLES <<")
    val variableCoordinates = sortedEigenvectors.t * standardizedMatrix.t
    printMatrix(variableCoordinates)

    // Paso 8: Calcular la matriz de calidades de las variables (mxm) -----------------------------------------------------------------------
    println("\n8. >> MATRIZ DE CALIDADES DE LAS VARIABLES <<")
    val qualityVariables = DenseVector(variableCoordinates(*, ::).map(col => math.pow(norm(col), 2)).toArray)
    println(qualityVariables.toArray.map(el => f"[$el%8.4f  ]").mkString(" "))

    // Paso 9: Calcular el vector de inercias de los ejes
    println("\n9. >> VECTOR DE INERCIAS DE LOS EJES <<")
    val inertiaVector = DenseVector.zeros[Double](breezeMatrix.cols)
    for(i <- 0 until breezeMatrix.cols){
      inertiaVector(i) = (100*sortedEigenvalues(i))/breezeMatrix.cols
    }
    println(inertiaVector.toArray.map(el => f"[$el%8.4f  ]").mkString(" "))

    // Paso 10: Graficar
    val labels = Array("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10")
    val pc1 = principalComponents(::, 0).toArray
    val pc2 = principalComponents(::, 1).toArray
    graphicPlaneMain(pc1, pc2, labels)
  }
}
