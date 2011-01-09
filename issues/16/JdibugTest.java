class JdibugTest {
  private static int doStuff(int input) {
    int a = input + 1;
    int b = input - 1;
    int result =  a + b - input;

    return result;
  }
  
  public static void main(String argv[]) {
    System.out.println("Result: " + doStuff(42));
  }
}
