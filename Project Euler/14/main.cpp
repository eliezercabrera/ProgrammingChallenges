#include <vector>
#include <iostream>

int main(int argc, char **argv) {
    
	const int number = 1000000;
	 
	int sequenceLength = 0;
	int startingNumber = 0;
	long sequence;
	 
	std::vector<int> cache(number + 1, -1);
	cache[1] = 1;
	 
	for (int i = 2; i <= number; i++) {
		sequence = i;
		int k = 0;
		while (sequence != 1 && sequence >= i) {
			k++;
			if ((sequence % 2) == 0) {
				sequence = sequence / 2;
			} else {
				sequence = sequence * 3 + 1;
			}
		}
		//Store result in cache
		cache[i] = k + cache[sequence];
	 
		//Check if sequence is the best solution
		if (cache[i] > sequenceLength) {
			sequenceLength = cache[i];
			startingNumber = i;
		}
	}
    return 0;
}