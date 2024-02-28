#include <bitset>
#include <chrono>
#include <cmath>
#include <ctime>
#include <iostream>
#include <locale.h>
#include <random>
#include <string>
#include <vector>

constexpr int ERROR_LEN = 1;
constexpr int ONE = 2;
constexpr int ZERO = 3;
constexpr int RAND = 4;


typedef uint32_t base;
constexpr base base_max = UINT32_MAX;

constexpr base base_b = (sizeof(base));
constexpr base sizeof_base = (sizeof(base) << 3);

using namespace std;

class Bfunc{
public:
	base sizet = 0;         
	base size = 0;
	base* coef = nullptr;

	Bfunc(string str) {
		base len_str = str.length();
		if (len_str > 0 && (len_str & (len_str - 1)) == 0){
		}
		else{
			cout << "Длина должна быть степенью 2!\n";
			exit(ERROR_LEN);
		}
		sizet = len_str;
		size = sizet / sizeof_base + ((sizet % sizeof_base) != 0);
		coef = new base[size];
		for (size_t i = 0; i < size; i++){
			coef[i] = 0;
		}
		for (size_t i = 0; i < sizet; i++){
			coef[size - (i >> (base_b + 1)) - 1] <<= 1;
			if (str[i] == '1'){
				coef[size - (i >> (base_b + 1)) - 1] |= 1;
			}
		}
	}

	//RAND - рандом
	//ONE - константа 1
	//ZERO - константа 0
	Bfunc(base variables = base_b, int t = RAND){
		sizet = 1 << variables;
		size = sizet / sizeof_base + ((sizet % sizeof_base) != 0);
		coef = new base[size];
		if (t == RAND){
			random_device rd;
			mt19937 gen(rd());
			if (sizet <= sizeof_base){
				coef[0] = 0;
				coef[0] |= gen() >> (sizeof_base - sizet);
			}
			else{
				for (size_t i = 0; i < size; i++){
					coef[i] = gen();
				}
			}
		}
		if (t == ONE){
			if (sizet <= sizeof_base){
				coef[0] = base_max;
				coef[0] = coef[0] >> (sizeof_base - sizet);
			}
			else{
				for (size_t i = 0; i < size; i++){
					coef[i] = base_max;
				}
			}
		}
		if (t == ZERO){
			for (size_t i = 0; i < size; i++){
				coef[i] = 0;
			}
		}
	}

	~Bfunc(){
		if (coef != nullptr){
			delete[] coef;
			coef = nullptr;
		}
	}

	Bfunc(const Bfunc& other){
		sizet = other.sizet;
		size = other.size;
		coef = new uint32_t[size];
		for (size_t i = 0; i < size; i++){
			coef[i] = other.coef[i];
		}
	}

	Bfunc& operator = (const Bfunc& other){
		if (this == &other){
			return *this;
		}
		if (coef != nullptr){
			delete[] coef;
		}
		sizet = other.sizet;
		size = other.size;
		coef = new uint32_t[size];
		for (size_t i = 0; i < size; i++){
			coef[i] = other.coef[i];
		}
		return *this;
	}
	bool operator == (const Bfunc other) {
		if (sizet != other.sizet) {
			return false;
		}
		for (size_t i = 0; i < size; i++) {
			if (other.coef[i] != coef[i]) {
				return false;
			}
		}
		return true;
	}
	bool operator != (const Bfunc other) {
		return !(*this == other);
	}

	friend ostream& operator <<(ostream& out, Bfunc& other){
		if (other.sizet <= sizeof_base){
			bitset<sizeof_base> st(other.coef[0]);
			string str = st.to_string();
			str.erase(0, size_t(sizeof_base) - other.sizet);
			out << str;
		}
		else{
			for (size_t i = 0; i < other.size; i++){
				std::bitset<sizeof_base> st(other.coef[other.size - i - 1]);
				std::string str = st.to_string();
				out << str;
			}
		}
		return out;
	}

	base weight_bfunc(){
		base res = 0;
		for (size_t i = 0; i < size; i++){
			base tmp = coef[i];
			tmp = (tmp & 0x55555555L) + ((tmp >> 1) & 0x55555555L);
			tmp = (tmp & 0x33333333L) + ((tmp >> 2) & 0x33333333L);
			tmp = (tmp + (tmp >> 4)) & 0x0F0F0F0FL;
			tmp = tmp + (tmp >> 8);
			res += uint8_t(tmp + (tmp >> 16)) & 0x3F;
		}
		return res;
	}

	//void printByVec(){
	//	for (size_t i = 0; i < size; i++){
	//		bitset<sizeof_base> s(coef[i]);
	//		cout << s.to_string() << " " << i << endl;
	//	}
	//}

	base mask(base i) {
		switch (i){
		case 0: return 0x55555555;
		case 1: return 0x33333333;
		case 2: return 0x0F0F0F0F;
		case 3: return 0x00FF00FF;
		case 4: return 0x0000FFFF;
		default:
			break;
		}
		return 0xFFFFFFFF;
	}

	base log2(base n){
		if (n <= 1) {
			return 0;
		}
		base result = 0;
		while (n > 1){
			n >>= 1;
			result += 1;
		}
		return result;
	}

	Bfunc mobius() {
		Bfunc mobius = *this;
		base m = log2(sizeof_base);
		for (base i = 0; i < size; i++){
			for (base j = 0; j < m; j++){
				mobius.coef[i] ^= (mobius.coef[i] >> (1 << j)) & mask(j);
			}
		}
		if (log2(sizet) < m){
			size_t bits = (1 << log2(sizet)) & (sizeof_base - 1);
			mobius.coef[0] &= (1 << bits) - 1;
			return mobius;
		}
		for (base i = 0; i < log2(sizet) - m; i++){
			size_t cs = 1 << i;
			for (base j = 0; j < log2(sizet) / cs; j += 2){
				for (base k = 0; k < cs; k++){
					mobius.coef[j * cs + k] ^= mobius.coef[(j + 1) * cs + k];
				}
			}
		}
		return mobius;
	}

	//string anf(){
	//	base len = log2(sizet);
	//	base pow = 0;
	//	Bfunc meb = this->mobius();
	//	uint8_t t = 0;
	//	string anf;
	//	Bfunc zero(len, ZERO);
	//	Bfunc one(len, ONE);
	//	if (meb == zero){
	//		anf += '0';
	//		return anf;
	//	}
	//	if (meb.sizet < sizeof_base){
	//		bitset<sizeof_base> st(meb.coef[0]);
	//		string str = st.to_string();
	//		str.erase(0, size_t(sizeof_base) - meb.sizet);
	//		if (str[0] == '1'){
	//			anf += "1 + ";
	//		}
	//		for (size_t i = 1; i < str.length(); i++){
	//			if (str[i] == '1'){
	//				bitset<sizeof_base> st2(i);
	//				string str2 = st2.to_string();
	//				str2.erase(0, size_t(sizeof_base) - len);
	//				for (size_t j = 0; j < str2.length(); j++){
	//					if (str2[j] == '1'){
	//						t++;
	//						anf += "x";
	//						anf += std::to_string((j + 1));
	//					}
	//				}
	//				if (t > pow){
	//					pow = t;
	//				}
	//				t = 0;
	//				if (anf[anf.length() - 2] != '+'){
	//					anf += " + ";
	//				}
	//			}
	//		}
	//	}
	//	else{
	//		if ((meb.coef[meb.size - 1] >> 31) & 1){
	//			anf += "1 + ";
	//		}
	//		for (size_t k = 0; k < meb.size; k++){
	//			bitset<sizeof_base> st(meb.coef[meb.size - k - 1]);
	//			string str = st.to_string();
	//			for (size_t i = 0; i < str.length(); i++){
	//				if (str[i] == '1'){
	//					bitset<sizeof_base> st2((i + (1 << 5) * k));
	//					string str2 = st2.to_string();
	//					str2.erase(0, size_t(sizeof_base) - len);
	//					for (size_t j = 0; j < str2.length(); j++){
	//						if (str2[j] == '1'){
	//							t++;
	//							anf += "x";
	//							anf += to_string((j + 1));
	//						}
	//					}
	//					if (t > pow) pow = t;
	//					t = 0;
	//					if (anf[anf.length() - 2] != '+') anf += " + ";
	//				}
	//			}
	//		}
	//	}
	//	if (anf[anf.length() - 2] == '+') anf.erase(anf.length() - 2, 1);
	//	return anf;
	//}


	string anf() {
		if (weight_bfunc() == 0){
			return "0";
		}
		int n = log2(sizet);
		string formula;
		for (base j = 0; j < size; j++){
			for (int i = 0; i < sizeof_base; i++) {
				if (((coef[j] >> (sizeof_base - 1 - i)) & 1) == 1) {
					if (!formula.empty()) {
						formula += " + ";
					}
					for (int k = 0; k < n; k++) {
						if (((i >> (n - k - 1)) & 1) == 1) {
							formula += "x" + to_string(k + 1);
						}
					}
					if (formula.empty()) {
						formula += "1";
					}
				}
			}
		}
		return formula;
	}

	vector<int> walsh_hadamard(){
		vector<int> walsh_hadamard(sizet);
		if (sizet < sizeof_base){
			for (size_t i = 0; i < sizet; i++){
				if (coef[0] & (1 << (sizet - i - 1))) walsh_hadamard[i] = -1;
				else walsh_hadamard[i] = 1;
			}
		}
		else{
			for (size_t i = 0; i < size; i++){
				for (size_t j = 0; j < sizeof_base; j++){
					if (coef[size - 1 - i] & (1 << (sizeof_base - j - 1))) walsh_hadamard[j + sizeof_base * i] = -1;
					else walsh_hadamard[j + sizeof_base * i] = 1;
				}
			}
		}
		base len = log2(sizet);
		for (base tmp1 = 0; tmp1 < len; tmp1++){
			for (base tmp2 = 0; tmp2 < (1 << (len - 1 - tmp1)); tmp2++){
				for (base i = tmp2 * (1 << (tmp1 + 1)), j = i + (1 << tmp1), mid = 0; mid < (1 << tmp1); mid++, i++, j++){
					walsh_hadamard[i] = walsh_hadamard[i] + walsh_hadamard[j];
				}
				for (base i = tmp2 * (1 << (tmp1 + 1)), j = i + (1 << tmp1), mid = 0; mid < (1 << tmp1); mid++, i++, j++){
					walsh_hadamard[j] = walsh_hadamard[i] - walsh_hadamard[j] - walsh_hadamard[j];
				}
			}
		}
		return walsh_hadamard;
	}

	base cor(){
		vector<int>wal = this->walsh_hadamard();
		base len = log2(wal.size()), temp_res = 0, temp = 0, temp_mid = 0, res = 0;
		for (size_t k = 1; k < (len + 1); k++){
			temp_res = ((1 << k) - 1) << (len - k);
			temp_mid = temp_res;
			base i, j = 0;
			while (true){
				if (wal[temp_res] != 0) return res;
				j = (temp_res + 1) & temp_res;
				temp = (j - 1) ^ temp_res;
				temp = (temp & 0x55555555L) + ((temp >> 1) & 0x55555555L);
				temp = (temp & 0x33333333L) + ((temp >> 2) & 0x33333333L);
				temp = (temp + (temp >> 4)) & 0x0F0F0F0FL;
				temp = temp + (temp >> 8);
				i = (uint8_t)(temp + (temp >> 16)) & 0x3F;
				i -= 2;
				temp_res = (((((temp_res + 1) ^ temp_res) << 1) + 1) << i) ^ j;
				if (temp_res > temp_mid) break;
				temp_mid = temp_res;
			}
			res++;
		}
		return res;
	}
};

int main() {
	setlocale(LC_ALL, "Russian");
	system("chcp 1251");

	//Bfunc b("10101110101011101010111010101110101011101010111010101110101011101010111010101110101011101010111010101110101011101010111010101110");
	//cout << b.mobius().anf() << "\n";

	



	//1 lab
	//for (size_t i = 2; i <= 5; i++) {
	//	Bfunc a(i, ONE);
	//	Bfunc b(i, ZERO);
	//	cout << a.weight_bfunc() << "\t" << b.weight_bfunc() << endl;
	//}
	//cout << endl << "10 случайных функций от 5 переменных:" << endl;
	//for (size_t i = 0; i < 10; i++)
	//{
	//	Bfunc a(5, RAND);
	//	cout << a << endl;
	//}
	//cout << endl << "Вес:" << endl;
	//for (size_t i = 2; i <= 31; i++)
	//{
	//	Bfunc x(i, RAND);
	//	uint64_t w; 
	//	w = x.weight_bfunc();
	//	std::cout << "Количество переменных = " << i << "\tw(f)/2^" << i << " = " << long double(w) / (uint64_t(1) << i) << "\n";
	//}

	//2 lab
	Bfunc a(6, ONE);
	cout << a << "\t" << "АНФ: " << a.mobius().anf() << endl;
	Bfunc b(6, ZERO);
	cout << b << "\t" << "АНФ: " << b.mobius().anf() << endl;
	for (size_t i = 2; i <= 30; i++){
	    Bfunc x(i, RAND);
	    Bfunc y(0), z(0);
	    y = x.mobius();
	    z = y.mobius();
	    cout << "Количество аргументов = " << i << "\tПреобразование Мёбиуса " << ((x == z) ? "верно!" : "не верно!");
	    cout << endl;
	}
	Bfunc x(31, RAND);
	Bfunc y(0), z(0);
	auto begin = chrono::steady_clock::now();
	y = x.mobius();
	auto end = chrono::steady_clock::now();
	auto t_mobius = chrono::duration_cast<chrono::milliseconds>(end - begin);	
	z = y.mobius();
	cout << "Количество аргументов = " << 31 << "\tПреобразование Мёбиуса " << ((x == z) ? "верно!" : "не верно!") << "\tВремя = " << t_mobius.count() << " мс\n";

	//3 lab
	//for (size_t i = 3; i <= 9; i += 3) {
	//	Bfunc one(i, ONE);
	//	vector<int> wal;
	//	wal = one.walsh_hadamard();
	//	cout << "Уолша — Адамара: ";
	//	for (auto j : wal) {
	//		cout << j << " ";
	//	}
	//	cout << endl;
	//}
	//cout << endl;
	//int i = 2;
	//while (true) {
	//	Bfunc rand(i, RAND);
	//	vector<int>wal;
	//	auto begin = chrono::steady_clock::now();
	//	wal = rand.walsh_hadamard();
	//	auto end = chrono::steady_clock::now();
	//	auto ttime = chrono::duration_cast<chrono::milliseconds>(end - begin);
	//	if (ttime.count() > 60000) {
	//		cout << "Количество аргументов = " << i << "\tВремя  =" << ttime.count() << " мс\n";
	//		cout << endl;
	//		break;
	//	}
	//	i++;
	//}


	return 0;
}