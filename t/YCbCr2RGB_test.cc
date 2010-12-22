// Dynamic service core for YCbCr to RGB conversion
// Based on code from IJG jpeg-7 library
#include <iostream>
#include <vector>

	#define c_MAXJSAMPLE 255
	#define c_TABSZ c_MAXJSAMPLE+2
using namespace std;
typedef vector<unsigned int> Word_List;



int fixp(float x, unsigned int scalebits)  {
    return (int)(x * (1<< scalebits) + 0.5);
}    


unsigned int range_limit(int x)  {
    if (x<0 ){
        return 0;
    } else if (x>255){
        return 255;
    } else {
        return x;
    }
}


void ds_YCbCr2RGB()  {

	Word_List result_list;


	cout << "DYN CORE YCbCr2RGB\n";
    Word_List     matrix1;
      	for(unsigned int i=0;i<256;i++) {
      	matrix1.push_back(i+(i<<8)+(i<<16));
      	}
      	
    const int c_CENTERJSAMPLE=128;
    const unsigned int c_SCALEBITS	= 16;
    const int c_ONE_HALF	=  1 << (c_SCALEBITS-1);
    
    int cr_r_tab[c_TABSZ];
    int cb_b_tab[c_TABSZ];
    int cr_g_tab[c_TABSZ];
    int cb_g_tab[c_TABSZ];
    
    int x = -c_CENTERJSAMPLE;
	for(unsigned int i=0;i<c_TABSZ;i++) {
		x=x+1;
		cr_r_tab[i] = (fixp(1.40200,c_SCALEBITS) * x + c_ONE_HALF)>> c_SCALEBITS;
		cb_b_tab[i] = (fixp(1.77200,c_SCALEBITS) * x + c_ONE_HALF)>> c_SCALEBITS;
		cr_g_tab[i] = -fixp(0.71414,c_SCALEBITS) * x;
		cb_g_tab[i] = -fixp(0.34414,c_SCALEBITS) * x + c_ONE_HALF;
  }    
    uint npix=matrix1.size();
	for(uint i=0;i<npix ;i++) {
		uint elt1= matrix1[i];
        uint y=elt1&255;
        uint cb=(elt1>>8)&255;
        uint cr=(elt1>>16)&255;
		unsigned int r =   range_limit( y + cr_r_tab[cr] );
		unsigned int g =  range_limit( y + ((cb_g_tab[cb] + cr_g_tab[cr])>>c_SCALEBITS) );
		unsigned int b =  range_limit( y + cb_b_tab[cb] );

		unsigned int w =  (r&255)+((g&255)<< 8)+((b&255)<< 16);
		result_list.push_back(w);
		        cout << "DYN YCbCr2RGB: "<<y<<","<<cb<<","<<cr<<"\t"<<fixp(1.40200,c_SCALEBITS)*(1-c_CENTERJSAMPLE)+c_ONE_HALF<<"\t"<<r<<","<<g<<","<<b<<"\n";
	}

//	return result_list;
} // of ds_YCbCr2RGB 

int main () {

ds_YCbCr2RGB();
return 1;

}

