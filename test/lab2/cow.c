int main() {
  int x = 5;
  int y = 0;
  int z = 1024 * 1024;
  while (x) {
      x -= 1;
      y += 5;
      z /= 2;
  }
  return y;
}
