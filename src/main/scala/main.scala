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

    def main(args: Array[String]): Unit = {
      //Leer archivo csv
      val filePath = System.getProperty("user.dir")+"/EjemploEstudiantes.csv"   
      val matrix = readAndProcessCSV(filePath)
      println("Matriz Original:")
      matrix.foreach(row => println(row.mkString(", ")))

      //Paso 1: Centrar y reducir la tabla original de datos X.
      println("\nMatriz Estandarizada: ")
      val breezeMatrix = DenseMatrix(matrix*)
      val meanVector = mean(breezeMatrix(::, *))
      val stddevVector = stddev(breezeMatrix(::, *))
      val standardizedMatrix = DenseMatrix.zeros[Double](breezeMatrix.rows, breezeMatrix.cols)

      for (col <- 0 until breezeMatrix.cols) {
        val colData = breezeMatrix(::, col)
        val standardizedCol = (colData - meanVector(col)) / stddevVector(col)
        standardizedMatrix(::, col) := standardizedCol
      }
      standardizedMatrix(*, ::).foreach(row => println(row.toArray.mkString(", ")))

      //Paso 2: Realizar el cálculo de la matriz de correlaciones
      println("\nMatriz Correlaciones: ")
      val covarianceMatrix = cov(standardizedMatrix)
      covarianceMatrix(*, ::).foreach(row => println(row.toArray.mkString(", ")))

      //Paso 3: Calcular y ordenar (de mayor a menor) los vectores y valores propios de la Matriz
      val eigResult = eig(covarianceMatrix)

      val eigenvalues = eigResult.eigenvalues
      val eigenvectors = eigResult.eigenvectors

      val sortedIndices = eigenvalues.toArray.zipWithIndex.sortBy(-_._1).map(_._2)
      val sortedEigenvalues = DenseVector(sortedIndices.map(i => eigenvalues(i)))

      val sortedEigenvectors = DenseMatrix.tabulate(eigenvectors.rows, eigenvectors.cols) { (i, j) =>
        eigenvectors(i, sortedIndices(j))
      }

      println("\nEigenvalores ordenados (de mayor a menor):")
      println(sortedEigenvalues)

      println("\nEigenvectores ordenados:")
      sortedEigenvectors(*, ::).foreach(row => println(row.toArray.mkString(", ")))

      //Paso 5: Calcular la matriz de componentes principales

      //Paso 6: Cálculo de la matriz de calidades de individuos 

      //Paso 7: Calcular la matriz de coordenada de las variables

      //Paso 8: Calcular la matriz de calidades de las variables

      //Paso 9: Calcular el vector de inercias de los ejes

      //Graficos
    }
}
