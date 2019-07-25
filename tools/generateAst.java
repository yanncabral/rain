import java.io.IOException;                                        
import java.io.PrintWriter;                                        
import java.util.Arrays;                                           
import java.util.List;                                             

public class generateAst {                                         

  private static void defineImplementations(                                
      PrintWriter writer, String baseName,                       
      String className, String fieldList) {                      
    writer.println("constructor T"+className+".Create(n" + fieldList.replace(", ","; n").replace(": ", ": T") + ");");
    writer.println("begin");

    String[] fields = fieldList.split(", ");                     
    for (String field : fields) {                                
      String name = field.split(" ")[0];                         
      writer.println("    self." + name.replace(":","") + " := n" + name.replace(":","") + ";"); 
    }                 

    writer.println("end;\n");
                                     
    writer.println("function T"+className+".Accept(Visitor: IVisitor): string;");
    writer.println("begin");
    writer.println("    exit(Visitor.visit(self));");                   
    writer.println("end;\n");  
  }    

  private static void defineType(                                
      PrintWriter writer, String baseName,                       
      String className, String fieldList) {                      
    writer.println("    T"+className+" = class(T"+baseName+")");

    String[] fields = fieldList.split(", ");   
    String properties = "";                  
    for (String field : fields) {                                
      String[] T = field.split(" ");
      properties += "        "+T[0]+" T"+T[1]+";\n";
    }  
    writer.println("        "+properties.trim());
    // Constructor.      
    writer.println("        function Accept(Visitor: IVisitor): string; override;");                                           
    writer.println("        constructor Create(n" + fieldList.replace(", ","; n").replace(": ", ": T") + ");");
    writer.println("    end;\n");
                               
  }                                                              

  private static void defineAst(                            
  String outputDir, String baseName, List<String> types)
  throws IOException {      

    String path = outputDir + "/" + baseName.toLowerCase() + ".pp";     
    PrintWriter writer = new PrintWriter(path, "UTF-8");

    writer.println("unit expr;");
    writer.println("{$mode objfpc}\n");
    writer.println("interface\n");                                       
    writer.println("uses token;\n");                                           
    writer.println("type");      
    writer.println("    TVariant = Variant;");     
    writer.println("    IVisitor = interface;");         
    
    writer.println("    T"+baseName+" = class ");    
    writer.println("        function Accept(Visitor: IVisitor): string; virtual; abstract;");
    writer.println("    end;");
    writer.println("    TExprArray = array of TExpr;\n");    


    for (String type : types) {                             
      String className = type.split(">")[0].trim();   
      String fields = type.split(">")[1].trim(); 
      defineType(writer, baseName, className, fields);      
    }   

    writer.println("    IVisitor = interface");
    for (String type : types) {                             
      String className = type.split(">")[0].trim();   
      writer.println("        function visit(expr: T"+className+"): string; overload; virtual; abstract;");
    }   
    writer.println("    end;");

    writer.println("\nimplementation\n");

    for (String type : types) {
      String className = type.split(">")[0].trim();   
      String fields = type.split(">")[1].trim(); 
      defineImplementations(writer, baseName, className, fields);         
    }
                      
    writer.print("end.");
    writer.close();                                         
}                                                         

  public static void main(String[] args) throws IOException {      
    if (args.length != 1) {                                        
      System.err.println("Usage: generateAst <output directory>");
      System.exit(1);                                              
    }                                                              
    String outputDir = args[0];  

    /* gramática livre de contexto */
    defineAst(outputDir, "Expr", Arrays.asList(          
      "Binary   > Left: Expr, Op: Token, Right: Expr",
      "Grouping > Expression: Expr",                      
      "Literal  > Value: Variant",                         
      "Unary    > Op: Token, Right: Expr"            
    ));         

  }                                                                
}                                  