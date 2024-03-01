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

base weight_base(base num) {
	base w = num;
	w = (w & 0x55555555L) + ((w >> 1) & 0x55555555L);
	w = (w & 0x33333333L) + ((w >> 2) & 0x33333333L);
	w = (w + (w >> 4)) & 0x0F0F0F0FL;
	w = w + (w >> 8);
	return (uint8_t)(w + (w >> 16)) & 0x3F;;
}
int mask(int i) {
	switch (i) {
	case 0: return 0x55555555;
	case 1: return 0x33333333;
	case 2: return 0x0F0F0F0F;
	case 3: return 0x00FF00FF;
	case 4: return 0x0000FFFF;
	default:
		break;
	}
	return 0x00000000;
}
base log2(base n) {
	if (n <= 1) {
		return 0;
	}
	base result = 0;
	while (n > 1) {
		n >>= 1;
		result += 1;
	}
	return result;
}

class Bfunc{
	base sizet = 0;         
	base size = 0;
	base* coef = nullptr;
public:
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
	bool operator == (const Bfunc& other) {
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
	bool operator != (const Bfunc& other) {
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

	Bfunc mobius() {
		Bfunc mobius = *this;
		int m = log2(sizeof_base);
		for (int i = 0; i < size; i++){
			for (int j = 0; j < m; j++){
				mobius.coef[i] ^= (mobius.coef[i] >> (1 << j)) & mask(j);
			}
		}
		if (log2(sizet) < m){
			uint32_t bits = (1 << log2(sizet)) & (sizeof_base - 1);
			mobius.coef[0] &= (1 << bits) - 1;
			return mobius;
		}
		uint32_t l_2 = log2(sizet);
		for (uint32_t k = 0; k < l_2 - 5; k++){
			for (uint32_t l = 0; l < (1 << (l_2 - 6 - k)); l++){
				for (uint32_t i = l * (1 << (k + 1)), j = i + (1 << k), p = 0; p < (1 << k); p++, i++, j++){
					mobius.coef[i] ^= mobius.coef[j];
				}
			}
		}
		return mobius;
	}

	//Bfunc mobius()
	//{
	//	Bfunc g(0);
	//	g = *this;
	//	if (sizet == 2)
	//	{
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 1) & 0x55555555);
	//		return g;
	//	}
	//	if (sizet == 4)
	//	{
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 1) & 0x55555555);
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 2) & 0x33333333);
	//		return g;
	//	}
	//	if (sizet == 8)
	//	{
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 1) & 0x55555555);
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 2) & 0x33333333);
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 4) & 0x0f0f0f0f);
	//		return g;
	//	}
	//	if (sizet == 16)
	//	{
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 1) & 0x55555555);
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 2) & 0x33333333);
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 4) & 0x0f0f0f0f);
	//		g.coef[0] = g.coef[0] ^ ((g.coef[0] >> 8) & 0x00ff00ff);
	//		return g;
	//	}
	//	if (sizet >= 32)
	//	{
	//		for (size_t i = 0; i < size; i++)
	//		{
	//			g.coef[i] ^= ((g.coef[i] >> 1) & 0x55555555);
	//			g.coef[i] ^= ((g.coef[i] >> 2) & 0x33333333);
	//			g.coef[i] ^= ((g.coef[i] >> 4) & 0x0f0f0f0f);
	//			g.coef[i] ^= ((g.coef[i] >> 8) & 0x00ff00ff);
	//			g.coef[i] ^= ((g.coef[i] >> 16) & 0x0000ffff);
	//		}
	//		uint32_t l_2 = log2(sizet);
	//		for (uint32_t k = 0; k < l_2 - 5; k++)
	//		{
	//			for (uint32_t l = 0; l < (1 << (l_2 - 6 - k)); l++)
	//			{
	//				for (uint32_t i = l * (1 << (k + 1)), j = i + (1 << k), p = 0; p < (1 << k); p++, i++, j++)
	//				{
	//					g.coef[i] ^= g.coef[j];
	//				}
	//			}
	//		}
	//		return g;
	//	}
	//	return  g;
	//}

	//1 - ANF
	//0 - DEG
	string anf(bool flag = true) {
		if (weight_bfunc() == 0){
			return "0";
		}
		int n = log2(sizet);
		string formula;
		base deg = 0;
		base deg_max = 0;
		for (base j = 0; j < size; j++){
			for (int i = 0; i < sizeof_base; i++) {
				if (((coef[j] >> (sizeof_base - 1 - i)) & 1) == 1) {
					if (!formula.empty()) {
						formula += " + ";
					}
					for (int k = 0; k < n; k++) {
						if (((i >> (n - k - 1)) & 1) == 1) {
							deg++;
							formula += "x" + to_string(k + 1);
						}
					}
					if (formula.empty()) {
						formula += "1";
					}
				}
				if (deg > deg_max) {
					deg_max = deg;
				}
				deg = 0;
			}
		}
		if (flag) return formula;
		else return to_string(deg_max);
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
		base n = log2(sizet);
		base a = 0, b = 0, c = 0;
		base result = 0;
		for (size_t k = 1; k <= n; k++){
			a = ((1 << k) - 1) << (n - k);
			base tmp_a = a;
			// a = a; tmp_a = i
			while (a <= tmp_a){
				tmp_a = a;
				if (wal[a] != 0) {
					return result;
				}
				b = (a + 1) & a;
				c = weight_base((b - 1) ^ a) - 2;
				a = (((((a + 1) ^ a) << 1) + 1) << c) ^ b;
			}
			result++;
		}
		return result;
	}

	base nonlinearity(){
		vector<int> wal = this->walsh_hadamard();
		auto minmax = minmax_element(wal.begin(), wal.end());
		base m_x = max(abs(*minmax.first), *minmax.second);
		return ((sizet >> 1) - ((m_x) >> 1));
	}

	string best_affine_approximation(){
		vector<int> wal = this->walsh_hadamard();
		base n = log2(sizet);
		int max_arg = 0;
		for (int i = 0; i < wal.size(); i++) {
			if (abs(wal[i]) > abs(wal[max_arg])) {
				max_arg = i;
			}
		}
		string res;
		if (wal[max_arg] < 0) {
			res = "1";
		}

		bitset<32> st2(max_arg);
		string str2 = st2.to_string();
		str2.erase(0, size_t(32) - n);
		for (size_t j = 0; j < str2.length(); j++){
			if (str2[j] == '1'){
				if (!res.empty()){
					res += " + ";
				}
				res += "X";
				res += std::to_string((j + 1));
			}
		}	
		if (res.empty()) {
			res = "0";
		}
		return res;
	}

	vector<int> autocor(){
		vector<int> autocor_vec = walsh_hadamard();
		for (int i = 0; i < autocor_vec.size(); i++){
			autocor_vec[i] *= autocor_vec[i];
		}
		for (int i = 0; i < log2(sizet); i++){
			size_t cs = 1 << i;
			for (int j = 0; j < (autocor_vec.size() / cs); j++){
				if ((j & 1) == 0) {
					for (int k = 0; k < cs; k++){
						autocor_vec[j * cs + k] += autocor_vec[(j + 1) * cs + k];
					}
				}
				else{
					for (int k = 0; k < cs; k++){
						autocor_vec[j * cs + k] = autocor_vec[(j - 1) * cs + k] - 2 * autocor_vec[j * cs + k];
					}
				}
			}
		}
		for (int i = 0; i < autocor_vec.size(); i++) {
			autocor_vec[i] >>= log2(sizet);
		}
		return autocor_vec;
	}

	base сomplete_nonlinear(){
		vector<int>autocor = this->autocor();
		auto minmax = minmax_element(autocor.begin() + 1, autocor.end());
		base m_x = max(abs(*minmax.first), *minmax.second);
		return (sizet >> 2) - ((m_x) >> 2);
	}
};

int main() {
	setlocale(LC_ALL, "Russian");
	system("chcp 1251");
	
	//string str = "0001000100011110000100010001111000010001000111101110111011100001";
	//Bfunc a(str);
	//Bfunc a(6, RAND);
	//Bfunc b = a.mobius();
	//cout << "f: " << a << "\n";
	//cout << "Mobius: " << b << "\n";
	//cout << "ANF: " << b.anf() << "\n";
	//cout << "Deg(f): " << b.anf(false) << "\n";
	//cout << "cor(f): " << a.cor() << "\n";
	//cout << "Walsh-Hadamard: ";
	//vector<int> wal;
	//wal = a.walsh_hadamard();
	//for (auto j : wal) {
	//	cout << j << " ";
	//}
	//cout << endl;
	//cout << "Nf: " << a.nonlinearity() << "\n";
	//cout << "NAP: " << a.best_affine_approximation() << "\n";
	//vector<int32_t> autocor;
	//cout << "Autocorrelation: ";
	//autocor = a.autocor();
	//for (auto j : autocor) {
	//	cout << j << " ";
	//}
	//cout << endl;
	//cout << "CNf: " << a.сomplete_nonlinear() << "\n";


	//Bfunc bf1("0111101001111010011110100111101001111010011110100111101001111010");
	//if (bf1.nonlinearity() != 8) {
	//	cout << "nonlinearity(01111010) != 1\n";
	//}
	//Bfunc bf2("0110000001100000011000000110000001100000011000000110000001100000");
	//if (bf2.nonlinearity() != 16) {
	//	cout << "nonlinearity(01100000) != 2\n";
	//}
	//Bfunc bf0("0000000000000000000000000000000000000000000000000000000000000000");
	//if (bf0.nonlinearity() != 0) {
	//	cout << "nonlinearity(0000000000000000000000000000000000000000000000000000000000000000) != 0\n";
	//}
	//Bfunc bf11("1111111111111111111111111111111111111111111111111111111111111111");
	//if (bf11.nonlinearity() != 0) {
	//	cout << "nonlinearity(1111111111111111111111111111111111111111111111111111111111111111) != 0\n";
	//}
	
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
	//Bfunc a(6, ONE);
	//Bfunc a_mob = a.mobius();
	//string a_anf = a_mob.anf();
	//string a_deg = a_mob.anf(false);
	//cout << a << "\t" << "АНФ: " << a_anf << " Степень: " << a_deg << endl;
	//Bfunc b(6, ZERO);
	//Bfunc b_mob = b.mobius();
	//string b_anf = b_mob.anf();
	//string b_deg = b_mob.anf(false);
	//cout << b << "\t" << "АНФ: " << b_anf << " Степень: " << b_deg << endl;
	//for (size_t i = 8; i <= 30; i++){
	//    Bfunc x(i, RAND);
	//    Bfunc y = x.mobius();
	//	Bfunc z = y.mobius();
	//	bool ans = (x == z);
	//    cout << "Количество аргументов = " << i << "\tПреобразование Мёбиуса " << (ans ? "верно!" : "не верно!")<< endl;
	//}
	//Bfunc x(31, RAND);
	//Bfunc y(0), z(0);
	//auto begin = chrono::steady_clock::now();
	//y = x.mobius();
	//auto end = chrono::steady_clock::now();
	//auto t_mobius = chrono::duration_cast<chrono::milliseconds>(end - begin);	
	//z = y.mobius();
	//cout << "Количество аргументов = " << 31 << "\tПреобразование Мёбиуса " << ((x == z) ? "верно!" : "не верно!") << "\tВремя = " << t_mobius.count() << " мс" << endl;

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

	//lab 4
	//Bfunc one(16, ONE);
	//cout << "cor(f(x1..x16) == 1) = " << one.cor() << "\n";
	//Bfunc x("01101001");
	//cout << "f = " << x << " cor = " << x.cor() << "\n";
	//int i = 2;
	//while (true) {
	//	Bfunc x(i, ZERO);
	//	base cor = 0;
	//	auto begin = chrono::steady_clock::now();
	//	cor = x.cor();
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