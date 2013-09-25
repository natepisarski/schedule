#include <iostream>
#include <fstream>
using namespace std;
int main(int argc, char**argv){
  ofstream file;
  file.open(argv[1]);
  for(int i = 2; i < argc; i++){
    file << string(argv[i]);
    file << endl;
  }
  file.close();
  return 0;
}
