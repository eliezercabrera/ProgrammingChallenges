#include <iostream>
#include <cmath>
#include <chrono>
#include <thread>


using namespace std;


namespace whatever // formatted for blog readability
{
	  typedef std::chrono::high_resolution_clock clock;
	  typedef std::chrono::microseconds microseconds;
	  typedef std::chrono::milliseconds milliseconds;
	 
	  clock::time_point now(){return clock::now();}
	 
	  microseconds intervalUs(const clock::time_point& t1, const clock::time_point& t0)
	 {
		return std::chrono::duration_cast<microseconds>(t1 - t0);
	 }
	 
	  milliseconds intervalMs(const clock::time_point& t1,const clock::time_point& t0)
	 {
		return std::chrono::duration_cast<milliseconds>(t1 - t0);
	 }
	 
	class StopWatch
	{
	   clock::time_point start_;
	 public:
	   StopWatch() : start_(clock::now()){}
	   clock::time_point restart() { start_ = clock::now(); return start_;}
	   microseconds elapsedUs()    { return intervalUs(now(), start_);}
	   milliseconds elapsedMs()    { return intervalMs(now(), start_);}
	};
	 
} // whatever

int main()
{
	whatever::StopWatch measure;
	int div = 0, sum = 0;

	for (int x = 1;; ++x) {
		int n = 1;
		sum += x;
		for (div = 2; div*div < sum; ++div) {
			if(sum % div == 0)
				++n;
		}
		
		if(n > 250)
			break;
	}
	auto elapsed_us = measure.elapsedMs().count();
	
	cout << "The first triangle number with over 500 divisors is: " << sum
		 << "\nIt took " << elapsed_us << " milliseconds.\n";

	return 0;
}