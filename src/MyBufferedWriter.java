import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

public class MyBufferedWriter extends BufferedWriter{
    public MyBufferedWriter(OutputStreamWriter osw) {
        super(osw);
    }

    public void writeLine(String line) throws IOException {
        this.write(line);
        this.newLine();
    }
	
	  public void writeLineJoin(String line) throws IOException {
        this.write(line);
        
    }

    public void writeNow(String line) throws IOException {
        this.write(line);
        this.newLine();
        this.flush();
    }

}
