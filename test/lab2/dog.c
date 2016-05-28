int main() {
  int x = 2;
  int y = 1024;
  while (x) {
      x -= 1;
      y /= 2;
  }
  return y;
}
