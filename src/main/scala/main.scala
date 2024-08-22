import com.opencsv.CSVReaderBuilder
import com.opencsv.CSVParserBuilder
import java.io.FileReader
import scala.jdk.CollectionConverters._
import breeze.linalg._
import breeze.stats._
import breeze.numerics._
import java.awt.{Color, BasicStroke}
import java.awt.geom.{Ellipse2D, Path2D}
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.data.xy._
import org.jfree.chart.annotations._
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.axis._

object MainApp {
  var labels: Array[String] = Array()
  var colName: Array[String] = Array()
  def readAndProcessCSV(filePath: String): Array[Array[Double]] = {
    val parser = new CSVParserBuilder()
      .withSeparator(';')
      .build()
    val reader = new CSVReaderBuilder(new FileReader(filePath))
      .withCSVParser(parser)
      .build()
    try {
      val allRows = reader.readAll().asScala.toList
      val dataRows = allRows.tail
      colName = allRows.head.tail
      labels = dataRows.map(row => row(0)).toArray
      val matrix = dataRows.map { row =>
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

  def graphicPlaneMain(x: Int, y: Int, principalComponents: DenseMatrix[Double]): Unit = {
    val pc1 = principalComponents(::, x).toArray
    val pc2 = principalComponents(::, y).toArray
    val dataset = new XYSeriesCollection()
    val series = new XYSeries("Alumnos")
    for (i <- pc1.indices) {
      series.add(pc1(i), pc2(i))
    }
    dataset.addSeries(series)

    val chart = ChartFactory.createScatterPlot(
      "Análisis de componentes principales",
      "Componente Principal "+x,
      "Componente Principal "+y,
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

  def graphicCorrelationCircle(correlations: DenseMatrix[Double], x: Int, y: Int): Unit = {
    val dataset = new XYSeriesCollection()
    val series = new XYSeries("Variables")
  
    for (i <- 0 until correlations.rows) {
      series.add(correlations(i, 0), correlations(i, 1))
    }
    dataset.addSeries(series)
  
    val chart = ChartFactory.createScatterPlot(
      "Círculo de Correlación",
      s"Componente Principal $x",
      s"Componente Principal $y",
      dataset
    )
  
    val plot = chart.getXYPlot
  
    val xAxis = plot.getDomainAxis.asInstanceOf[NumberAxis]
    val yAxis = plot.getRangeAxis.asInstanceOf[NumberAxis]
    xAxis.setRange(-1.0, 1.0)
    yAxis.setRange(-1.0, 1.0)
  
    plot.setDomainZeroBaselineVisible(true)
    plot.setRangeZeroBaselineVisible(true)
    plot.setDomainPannable(true)
    plot.setRangePannable(true)
    plot.setDomainGridlinePaint(Color.LIGHT_GRAY)
    plot.setRangeGridlinePaint(Color.LIGHT_GRAY)
  
    val circleRadius = 1.0
    val circleShape = new Ellipse2D.Double(-circleRadius, -circleRadius, 2 * circleRadius, 2 * circleRadius)
    val circleAnnotation = new XYShapeAnnotation(circleShape,
      new BasicStroke(1.0f), Color.BLACK
    )
    plot.addAnnotation(circleAnnotation)
  
    for (i <- 0 until correlations.rows) {
      val startX = 0.0
      val startY = 0.0
      val endX = correlations(i, 0)
      val endY = correlations(i, 1)
      val line = new XYLineAnnotation(startX, startY, endX, endY, new BasicStroke(1.0f), Color.BLUE)
      plot.addAnnotation(line)
      val annotation = new XYTextAnnotation(colName(i), endX * 1.1, endY * 1.1)
      plot.addAnnotation(annotation)
    }
  
    val frame = new ChartFrame("Círculo de Correlación", chart)
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

    val squaredEigenvectors = pow(sortedEigenvectors, 2.0)
    val norms = sqrt(sum(squaredEigenvectors,  breeze.linalg.Axis._1))

    val variableCoordinates = DenseMatrix.zeros[Double](sortedEigenvectors.rows, sortedEigenvectors.cols)
    for (i <- 0 until sortedEigenvectors.rows) {
      for (j <- 0 until sortedEigenvectors.cols) {
        variableCoordinates(i, j) = sortedEigenvectors(i, j) * norms(i)
      }
    }

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
    var x:Int = 0
    var y:Int = 1
    for(i <- 0 until 2){
      if(i == 1){
        x= 3
        y = 4
      }
      graphicPlaneMain(x, y, principalComponents)
      graphicCorrelationCircle(variableCoordinates(::, x until y + 1),x,y)
    }
  }
}
