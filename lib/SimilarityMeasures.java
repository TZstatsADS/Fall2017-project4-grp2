import java.util.*;
import java.io.*;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.stat.correlation.*;

public class SimilarityMeasures {

    public static final void main(String[] args) throws FileNotFoundException, IOException{
        
        File inFile  = new File( args[0] );
        int nrow = Integer.parseInt( args[1] );
        int ncol = Integer.parseInt( args[2] );
        Boolean writeCsv  = true;
        Boolean spearCorr = true;
        Boolean vecSim    = true;

        try{

        		// read input data
	    		// instantiate matrix to perform calculation
			double[][] dataMatrix = new double[ ncol-1 ][ nrow-1 ];
		    String[] rowOfMatrix = new String[ncol];
		
		    Scanner scanner = new Scanner( inFile );
		    scanner.useDelimiter(",");
		
		    // skip header
		    scanner.nextLine();
		    
		    // get users line	    
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
            System.out.println("Original data dimensions");
            System.out.println( "nrows = " + dataMatrix.length );
            System.out.println( "ncols = " + dataMatrix[0].length );
            System.out.println("");
            
            if( spearCorr && vecSim && writeCsv) {
            	
            		// ---------------------------------
                // SPEARMAN CORRELATION
                // ---------------------------------
            	
            		System.out.println("Computing Spearman Correlation...");
            		// instantiate object SpearmansCorrelation
                SpearmansCorrelation sc = new SpearmansCorrelation();
                
                // compute spearman correlation coefficient
                RealMatrix m = sc.computeCorrelationMatrix( dataMatrix );
                
                
                // ---------------------------------
                // VECTOR (COSINE) SIMILARITY
                // ---------------------------------                
	        		System.out.println("Computing cosine similarity...");
	        		double[][] sim = cosineSimilarity( dataMatrix );
	        		
	        		System.out.println("Cosine similarity matrix dimensions");
	            System.out.println( "nrows = " + sim.length );
	            System.out.println( "ncols = " + sim[0].length );
	            System.out.println("");

            	
	            // ---------------------------------
                // WRITE CSV FILE
                // ---------------------------------
	            
            	 	// write correlation matrix to csv file
                FileWriter writer = new FileWriter("similarity_measures.csv");
                
                System.out.println("Writing csv file...");
                
                // write header
                writer.write("i,j,spear_corr,vec_sim");
                writer.append('\n');
                
                // for each row
                for( int iRow = 0; iRow < nrow-1; iRow++ ) {
                	// for each column
                		for( int iCol = iRow+1; iCol < nrow-1; iCol++ ) {
                			
                			// write line in file
                			writer.append( users[ iRow ] );
                			writer.append(',');
                			writer.append( users[ iCol ] );
                			writer.append(',');
                			writer.write( Double.toString( m.getEntry( iRow, iCol ) ) );
                			writer.append(',');
                			writer.write( Double.toString( sim[iRow][iCol] ) );
                			writer.append('\n');
                			
                		}
                		
                }

                writer.flush();
                writer.close();
                
                System.out.println("done!");
            	
            }
                   
            
            if( vecSim ) {
            	
            	
            }
            

        }
        catch(FileNotFoundException e){

            System.out.println("Exception: " + e);

        }


    }
    
    // compute cosine similarity between columns of matrix
    private static double[][] cosineSimilarity( double[][] data ){
    	
    		int nObs   = data.length;
    		int nUsers = data[0].length;
    		double[][] sim = new double[ nUsers ][ nUsers ];
    		
    		double prod = 0.0;
    		double norm1 = 0.0;
    		double norm2 = 0.0;
    		
    		// cos(v1,v2) = dot.product(v1,v2) / (|v1| |v2|)
    		
        // step 1 : matrix multiplication
        for( int i = 0; i < nUsers; i++ ) {
        	
        		for( int j = 0; j < nUsers; j++ ) {
        			
        			prod = 0.0;
        			norm1 = 0.0;
        			norm2 = 0.0;
        			for( int k = 0; k < nObs; k++ ) { // AQUI TEM ERRO! CONSERTAR.....
        				
        				norm1 += data[k][i];
        				norm2 += data[k][j];
        				prod  += data[k][i] * data[k][j];
        				
        			}
        			
        			sim[i][j] = prod / ( norm1 * norm2 );
        			
        		}
        }
    		
    		return (sim); 
    		
    }

//    // method for reading input data
//    private static double[][] readInputData ( File inFile, int nrow, int ncol ) 
//    		throws FileNotFoundException {
//    	
//    		try { 
//	    		// instantiate matrix to perform calculation
//			double[][] dataMatrix = new double[ ncol-1 ][ nrow-1 ];
//		    String[] rowOfMatrix = new String[ncol];
//		
//		    Scanner scanner = new Scanner( inFile );
//		    scanner.useDelimiter(",");
//		
//		    // skip header
//		    scanner.nextLine();
//		    
//		    // get users line	    
//		    String[] users = new String[ nrow - 1 ];
//		
//		    int i = 0;
//		    // while there's data in line
//		    while(scanner.hasNext()){
//		        
//		        rowOfMatrix = scanner.nextLine().split(",");
//		        
//		        // store user ID
//		        users[ i ] = rowOfMatrix[ 0 ];
//		
//		        i++;
//		        for( int j = 1; j < rowOfMatrix.length; j++ ){
//		        	
//		            dataMatrix[j-1][i-1] = (double) Double.parseDouble(rowOfMatrix[j]);
//		
//		        }
//		
//		    }
//		    scanner.close();
//		    
//		    return( dataMatrix );
//		    
//    		}
//    		catch( FileNotFoundException e ){
//                System.out.println("Exception: " + e);
//    		}
//    	
//    }
    
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