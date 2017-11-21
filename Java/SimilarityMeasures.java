import java.util.*;
import java.io.*;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.stat.correlation.*;//SpearmansCorrelation;

public class SimilarityMeasures {

    public static final void main(String[] args) throws FileNotFoundException, IOException{
        
        File inFile  = new File( args[0] );
        int nrow = Integer.parseInt( args[1] );
        int ncol = Integer.parseInt( args[2] );

        try{

            // instantiate matrix to perform calculation
        		double[][] dataMatrix = new double[ ncol-1 ][ nrow-1 ];
            String[] rowOfMatrix = new String[ncol];

            Scanner scanner = new Scanner( inFile );
            scanner.useDelimiter(",");

            // read header line
            String[] header = new String[ncol];
            header = scanner.nextLine().split(",");
            
            String[] users = new String[ nrow - 1 ];

            int i = 0;
            // while there's data in line
            while(scanner.hasNext()){
                
                rowOfMatrix = scanner.nextLine().split(",");
                
                // store user ID
                users[ i ] = rowOfMatrix[ 0 ];

                i++;
                for( int j = 1; j < rowOfMatrix.length; j++ ){
                	
                    dataMatrix[j-1][i-1] = (double) Double.parseDouble(rowOfMatrix[j]);

                }

            }
            scanner.close();        

            // print original matrix dimensions
            System.out.println("original data dimensions");
            System.out.println( "nrows = " + dataMatrix.length );
            System.out.println( "ncols = " + dataMatrix[0].length );
            System.out.println("");

            System.out.println("compute spearman correlation coeffcient");
            
            // instantiate object SpearmansCorrelation
            SpearmansCorrelation sc = new SpearmansCorrelation();
            
            // compute spearman correlation coefficient
            RealMatrix m = sc.computeCorrelationMatrix( dataMatrix );
            
            //double[][] matCorr = m.getData();
            
            // write correlation matrix to csv file
            FileWriter writer = new FileWriter("spearman_correlation.csv");
            
            System.out.println("Writing csv file...");
            
            // write header
            writer.write("i,j,corr");
            writer.append('\n');
            
            // for each row
            for( int iRow = 0; iRow < 4151; iRow++ ) {
            	// for each column
            		for( int iCol = iRow+1; iCol < 4151; iCol++ ) {
            			
            			// write line in file
            			writer.append( users[ iRow ] );
            			writer.append(',');
            			writer.append( users[ iCol ] );
            			writer.append(',');
            			writer.write( Double.toString( m.getEntry( iRow, iCol ) ) );
            			writer.append('\n');
            			
            			System.out.println("saiu do loop");
            			
            		}
            		
            }

            writer.flush();
            writer.close();
                   
            System.out.println( header[0] + " - " + header[1] + "" + header.length );

            System.out.println("done!");

        }
        catch(FileNotFoundException e){

            System.out.println("Exception: " + e);

        }


    }

    // method to tranpose matrix
    public static double[][] transposeMatrix(double[][] matrix)
    {

        int m = matrix.length;
        int n = matrix[0].length;

        double[][] transposedMatrix = new double[n][m];

        for(int x = 0; x < n; x++)
        {
            for(int y = 0; y < m; y++)
            {
                transposedMatrix[x][y] = matrix[y][x];
            }
        }

        return transposedMatrix;
    }
     

}