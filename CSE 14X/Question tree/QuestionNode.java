//Soham Ghose 1663396
//CSE HW 3 section AG

// This program creates a object called questionNode that can store data and 
// stores refernces to two other questionNodes
public class QuestionNode{
   // This field stores the question or answer 
   public String data ; 
   // stores refernce to the questionNode if
   // the answer to question is yes
   public QuestionNode left ; 
   // stores reference to the questionNode if 
   //answer to question is no
   public QuestionNode right ; 
   // stores true or false depending on type of 
   // data in questionNode
   public boolean isLeaf ; 
   // constructor
   // takes in a string that can be a question or an object 
   // initialises new questionNode with provided string and null references. 
   public QuestionNode(String question){
      this(question, null, null);   
   }
   // constructor 
   // takes in a string that is the question or an object and two questionNode references. 
   // initialises new questionNode with input string and two references. 
   public QuestionNode(String question, QuestionNode left, QuestionNode right){
      this.right = right ;
      this.left = left ; 
      this.data = question ;
      isLeaf = (right == null && left == null) ;
   }

}