/*  Following Java program is hand-converted from the C-version that
 * was automatically generated from Cryptol via the SBV backend for
 * 128-bit AES encryption.
 * 
 *  This is only for demo purposes: If we go thru the SBV backend
 * to Java, the final output will likely look similar but will not be
 * exactly the same. This program is only meant as a demonstration
 * of what can be automatically generated, if we choose to do so.
 *
 */

class AES128Encrypt {
 static public short grab8(int v) {
   return (short) (v & 0xFF);
 }

 static public int[] encrypt (
         int s0_arr[]    /* Key: [128 = 4*32] */,
         int s1_arr[]    /* PT : [128 = 4*32] */
	 )
 {
  int table0[] = {0xa56363c6, 0x847c7cf8, 0x997777ee, 0x8d7b7bf6,
                   0x0df2f2ff, 0xbd6b6bd6, 0xb16f6fde, 0x54c5c591, 
                   0x50303060, 0x03010102, 0xa96767ce, 0x7d2b2b56, 
                   0x19fefee7, 0x62d7d7b5, 0xe6abab4d, 0x9a7676ec,
                   0x45caca8f, 0x9d82821f, 0x40c9c989, 0x877d7dfa, 
                   0x15fafaef, 0xeb5959b2, 0xc947478e, 0x0bf0f0fb, 
                   0xecadad41, 0x67d4d4b3, 0xfda2a25f, 0xeaafaf45,
                   0xbf9c9c23, 0xf7a4a453, 0x967272e4, 0x5bc0c09b, 
                   0xc2b7b775, 0x1cfdfde1, 0xae93933d, 0x6a26264c, 
                   0x5a36366c, 0x413f3f7e, 0x02f7f7f5, 0x4fcccc83,
                   0x5c343468, 0xf4a5a551, 0x34e5e5d1, 0x08f1f1f9, 
                   0x937171e2, 0x73d8d8ab, 0x53313162, 0x3f15152a, 
                   0x0c040408, 0x52c7c795, 0x65232346, 0x5ec3c39d,
                   0x28181830, 0xa1969637, 0x0f05050a, 0xb59a9a2f, 
                   0x0907070e, 0x36121224, 0x9b80801b, 0x3de2e2df, 
                   0x26ebebcd, 0x6927274e, 0xcdb2b27f, 0x9f7575ea,
                   0x1b090912, 0x9e83831d, 0x742c2c58, 0x2e1a1a34, 
                   0x2d1b1b36, 0xb26e6edc, 0xee5a5ab4, 0xfba0a05b, 
                   0xf65252a4, 0x4d3b3b76, 0x61d6d6b7, 0xceb3b37d,
                   0x7b292952, 0x3ee3e3dd, 0x712f2f5e, 0x97848413, 
                   0xf55353a6, 0x68d1d1b9, 0x00000000, 0x2cededc1, 
                   0x60202040, 0x1ffcfce3, 0xc8b1b179, 0xed5b5bb6,
                   0xbe6a6ad4, 0x46cbcb8d, 0xd9bebe67, 0x4b393972, 
                   0xde4a4a94, 0xd44c4c98, 0xe85858b0, 0x4acfcf85, 
                   0x6bd0d0bb, 0x2aefefc5, 0xe5aaaa4f, 0x16fbfbed,
                   0xc5434386, 0xd74d4d9a, 0x55333366, 0x94858511, 
                   0xcf45458a, 0x10f9f9e9, 0x06020204, 0x817f7ffe, 
                   0xf05050a0, 0x443c3c78, 0xba9f9f25, 0xe3a8a84b,
                   0xf35151a2, 0xfea3a35d, 0xc0404080, 0x8a8f8f05, 
                   0xad92923f, 0xbc9d9d21, 0x48383870, 0x04f5f5f1, 
                   0xdfbcbc63, 0xc1b6b677, 0x75dadaaf, 0x63212142,
                   0x30101020, 0x1affffe5, 0x0ef3f3fd, 0x6dd2d2bf, 
                   0x4ccdcd81, 0x140c0c18, 0x35131326, 0x2fececc3, 
                   0xe15f5fbe, 0xa2979735, 0xcc444488, 0x3917172e,
                   0x57c4c493, 0xf2a7a755, 0x827e7efc, 0x473d3d7a, 
                   0xac6464c8, 0xe75d5dba, 0x2b191932, 0x957373e6, 
                   0xa06060c0, 0x98818119, 0xd14f4f9e, 0x7fdcdca3,
                   0x66222244, 0x7e2a2a54, 0xab90903b, 0x8388880b, 
                   0xca46468c, 0x29eeeec7, 0xd3b8b86b, 0x3c141428, 
                   0x79dedea7, 0xe25e5ebc, 0x1d0b0b16, 0x76dbdbad,
                   0x3be0e0db, 0x56323264, 0x4e3a3a74, 0x1e0a0a14, 
                   0xdb494992, 0x0a06060c, 0x6c242448, 0xe45c5cb8, 
                   0x5dc2c29f, 0x6ed3d3bd, 0xefacac43, 0xa66262c4,
                   0xa8919139, 0xa4959531, 0x37e4e4d3, 0x8b7979f2, 
                   0x32e7e7d5, 0x43c8c88b, 0x5937376e, 0xb76d6dda, 
                   0x8c8d8d01, 0x64d5d5b1, 0xd24e4e9c, 0xe0a9a949,
                   0xb46c6cd8, 0xfa5656ac, 0x07f4f4f3, 0x25eaeacf, 
                   0xaf6565ca, 0x8e7a7af4, 0xe9aeae47, 0x18080810, 
                   0xd5baba6f, 0x887878f0, 0x6f25254a, 0x722e2e5c,
                   0x241c1c38, 0xf1a6a657, 0xc7b4b473, 0x51c6c697, 
                   0x23e8e8cb, 0x7cdddda1, 0x9c7474e8, 0x211f1f3e, 
                   0xdd4b4b96, 0xdcbdbd61, 0x868b8b0d, 0x858a8a0f,
                   0x907070e0, 0x423e3e7c, 0xc4b5b571, 0xaa6666cc, 
                   0xd8484890, 0x05030306, 0x01f6f6f7, 0x120e0e1c, 
                   0xa36161c2, 0x5f35356a, 0xf95757ae, 0xd0b9b969,
                   0x91868617, 0x58c1c199, 0x271d1d3a, 0xb99e9e27, 
                   0x38e1e1d9, 0x13f8f8eb, 0xb398982b, 0x33111122, 
                   0xbb6969d2, 0x70d9d9a9, 0x898e8e07, 0xa7949433,
                   0xb69b9b2d, 0x221e1e3c, 0x92878715, 0x20e9e9c9, 
                   0x49cece87, 0xff5555aa, 0x78282850, 0x7adfdfa5, 
                   0x8f8c8c03, 0xf8a1a159, 0x80898909, 0x170d0d1a,
                   0xdabfbf65, 0x31e6e6d7, 0xc6424284, 0xb86868d0, 
                   0xc3414182, 0xb0999929, 0x772d2d5a, 0x110f0f1e, 
                   0xcbb0b07b, 0xfc5454a8, 0xd6bbbb6d, 0x3a16162c};

  int table1[] = {0x6363c6a5, 0x7c7cf884, 0x7777ee99, 0x7b7bf68d,
                   0xf2f2ff0d, 0x6b6bd6bd, 0x6f6fdeb1, 0xc5c59154,
                   0x30306050, 0x01010203, 0x6767cea9, 0x2b2b567d,
                   0xfefee719, 0xd7d7b562, 0xabab4de6, 0x7676ec9a,
                   0xcaca8f45, 0x82821f9d, 0xc9c98940, 0x7d7dfa87,
                   0xfafaef15, 0x5959b2eb, 0x47478ec9, 0xf0f0fb0b,
                   0xadad41ec, 0xd4d4b367, 0xa2a25ffd, 0xafaf45ea,
                   0x9c9c23bf, 0xa4a453f7, 0x7272e496, 0xc0c09b5b,
                   0xb7b775c2, 0xfdfde11c, 0x93933dae, 0x26264c6a,
                   0x36366c5a, 0x3f3f7e41, 0xf7f7f502, 0xcccc834f,
                   0x3434685c, 0xa5a551f4, 0xe5e5d134, 0xf1f1f908,
                   0x7171e293, 0xd8d8ab73, 0x31316253, 0x15152a3f,
                   0x0404080c, 0xc7c79552, 0x23234665, 0xc3c39d5e,
                   0x18183028, 0x969637a1, 0x05050a0f, 0x9a9a2fb5,
                   0x07070e09, 0x12122436, 0x80801b9b, 0xe2e2df3d,
                   0xebebcd26, 0x27274e69, 0xb2b27fcd, 0x7575ea9f,
                   0x0909121b, 0x83831d9e, 0x2c2c5874, 0x1a1a342e,
                   0x1b1b362d, 0x6e6edcb2, 0x5a5ab4ee, 0xa0a05bfb,
                   0x5252a4f6, 0x3b3b764d, 0xd6d6b761, 0xb3b37dce,
                   0x2929527b, 0xe3e3dd3e, 0x2f2f5e71, 0x84841397,
                   0x5353a6f5, 0xd1d1b968, 0x00000000, 0xededc12c,
                   0x20204060, 0xfcfce31f, 0xb1b179c8, 0x5b5bb6ed,
                   0x6a6ad4be, 0xcbcb8d46, 0xbebe67d9, 0x3939724b,
                   0x4a4a94de, 0x4c4c98d4, 0x5858b0e8, 0xcfcf854a,
                   0xd0d0bb6b, 0xefefc52a, 0xaaaa4fe5, 0xfbfbed16,
                   0x434386c5, 0x4d4d9ad7, 0x33336655, 0x85851194,
                   0x45458acf, 0xf9f9e910, 0x02020406, 0x7f7ffe81,
                   0x5050a0f0, 0x3c3c7844, 0x9f9f25ba, 0xa8a84be3,
                   0x5151a2f3, 0xa3a35dfe, 0x404080c0, 0x8f8f058a,
                   0x92923fad, 0x9d9d21bc, 0x38387048, 0xf5f5f104,
                   0xbcbc63df, 0xb6b677c1, 0xdadaaf75, 0x21214263,
                   0x10102030, 0xffffe51a, 0xf3f3fd0e, 0xd2d2bf6d,
                   0xcdcd814c, 0x0c0c1814, 0x13132635, 0xececc32f,
                   0x5f5fbee1, 0x979735a2, 0x444488cc, 0x17172e39,
                   0xc4c49357, 0xa7a755f2, 0x7e7efc82, 0x3d3d7a47,
                   0x6464c8ac, 0x5d5dbae7, 0x1919322b, 0x7373e695,
                   0x6060c0a0, 0x81811998, 0x4f4f9ed1, 0xdcdca37f,
                   0x22224466, 0x2a2a547e, 0x90903bab, 0x88880b83,
                   0x46468cca, 0xeeeec729, 0xb8b86bd3, 0x1414283c,
                   0xdedea779, 0x5e5ebce2, 0x0b0b161d, 0xdbdbad76,
                   0xe0e0db3b, 0x32326456, 0x3a3a744e, 0x0a0a141e,
                   0x494992db, 0x06060c0a, 0x2424486c, 0x5c5cb8e4,
                   0xc2c29f5d, 0xd3d3bd6e, 0xacac43ef, 0x6262c4a6,
                   0x919139a8, 0x959531a4, 0xe4e4d337, 0x7979f28b,
                   0xe7e7d532, 0xc8c88b43, 0x37376e59, 0x6d6ddab7,
                   0x8d8d018c, 0xd5d5b164, 0x4e4e9cd2, 0xa9a949e0,
                   0x6c6cd8b4, 0x5656acfa, 0xf4f4f307, 0xeaeacf25,
                   0x6565caaf, 0x7a7af48e, 0xaeae47e9, 0x08081018,
                   0xbaba6fd5, 0x7878f088, 0x25254a6f, 0x2e2e5c72,
                   0x1c1c3824, 0xa6a657f1, 0xb4b473c7, 0xc6c69751,
                   0xe8e8cb23, 0xdddda17c, 0x7474e89c, 0x1f1f3e21,
                   0x4b4b96dd, 0xbdbd61dc, 0x8b8b0d86, 0x8a8a0f85,
                   0x7070e090, 0x3e3e7c42, 0xb5b571c4, 0x6666ccaa,
                   0x484890d8, 0x03030605, 0xf6f6f701, 0x0e0e1c12,
                   0x6161c2a3, 0x35356a5f, 0x5757aef9, 0xb9b969d0,
                   0x86861791, 0xc1c19958, 0x1d1d3a27, 0x9e9e27b9,
                   0xe1e1d938, 0xf8f8eb13, 0x98982bb3, 0x11112233,
                   0x6969d2bb, 0xd9d9a970, 0x8e8e0789, 0x949433a7,
                   0x9b9b2db6, 0x1e1e3c22, 0x87871592, 0xe9e9c920,
                   0xcece8749, 0x5555aaff, 0x28285078, 0xdfdfa57a,
                   0x8c8c038f, 0xa1a159f8, 0x89890980, 0x0d0d1a17,
                   0xbfbf65da, 0xe6e6d731, 0x424284c6, 0x6868d0b8,
                   0x414182c3, 0x999929b0, 0x2d2d5a77, 0x0f0f1e11,
                   0xb0b07bcb, 0x5454a8fc, 0xbbbb6dd6, 0x16162c3a};

  int table2[] = {0x63c6a563, 0x7cf8847c, 0x77ee9977, 0x7bf68d7b,
                   0xf2ff0df2, 0x6bd6bd6b, 0x6fdeb16f, 0xc59154c5, 
                   0x30605030, 0x01020301, 0x67cea967, 0x2b567d2b, 
                   0xfee719fe, 0xd7b562d7, 0xab4de6ab, 0x76ec9a76,
                   0xca8f45ca, 0x821f9d82, 0xc98940c9, 0x7dfa877d, 
                   0xfaef15fa, 0x59b2eb59, 0x478ec947, 0xf0fb0bf0, 
                   0xad41ecad, 0xd4b367d4, 0xa25ffda2, 0xaf45eaaf,
                   0x9c23bf9c, 0xa453f7a4, 0x72e49672, 0xc09b5bc0, 
                   0xb775c2b7, 0xfde11cfd, 0x933dae93, 0x264c6a26, 
                   0x366c5a36, 0x3f7e413f, 0xf7f502f7, 0xcc834fcc,
                   0x34685c34, 0xa551f4a5, 0xe5d134e5, 0xf1f908f1, 
                   0x71e29371, 0xd8ab73d8, 0x31625331, 0x152a3f15, 
                   0x04080c04, 0xc79552c7, 0x23466523, 0xc39d5ec3,
                   0x18302818, 0x9637a196, 0x050a0f05, 0x9a2fb59a, 
                   0x070e0907, 0x12243612, 0x801b9b80, 0xe2df3de2, 
                   0xebcd26eb, 0x274e6927, 0xb27fcdb2, 0x75ea9f75,
                   0x09121b09, 0x831d9e83, 0x2c58742c, 0x1a342e1a, 
                   0x1b362d1b, 0x6edcb26e, 0x5ab4ee5a, 0xa05bfba0, 
                   0x52a4f652, 0x3b764d3b, 0xd6b761d6, 0xb37dceb3,
                   0x29527b29, 0xe3dd3ee3, 0x2f5e712f, 0x84139784, 
                   0x53a6f553, 0xd1b968d1, 0x00000000, 0xedc12ced, 
                   0x20406020, 0xfce31ffc, 0xb179c8b1, 0x5bb6ed5b,
                   0x6ad4be6a, 0xcb8d46cb, 0xbe67d9be, 0x39724b39, 
                   0x4a94de4a, 0x4c98d44c, 0x58b0e858, 0xcf854acf, 
                   0xd0bb6bd0, 0xefc52aef, 0xaa4fe5aa, 0xfbed16fb,
                   0x4386c543, 0x4d9ad74d, 0x33665533, 0x85119485, 
                   0x458acf45, 0xf9e910f9, 0x02040602, 0x7ffe817f, 
                   0x50a0f050, 0x3c78443c, 0x9f25ba9f, 0xa84be3a8,
                   0x51a2f351, 0xa35dfea3, 0x4080c040, 0x8f058a8f, 
                   0x923fad92, 0x9d21bc9d, 0x38704838, 0xf5f104f5, 
                   0xbc63dfbc, 0xb677c1b6, 0xdaaf75da, 0x21426321,
                   0x10203010, 0xffe51aff, 0xf3fd0ef3, 0xd2bf6dd2, 
                   0xcd814ccd, 0x0c18140c, 0x13263513, 0xecc32fec, 
                   0x5fbee15f, 0x9735a297, 0x4488cc44, 0x172e3917,
                   0xc49357c4, 0xa755f2a7, 0x7efc827e, 0x3d7a473d, 
                   0x64c8ac64, 0x5dbae75d, 0x19322b19, 0x73e69573, 
                   0x60c0a060, 0x81199881, 0x4f9ed14f, 0xdca37fdc,
                   0x22446622, 0x2a547e2a, 0x903bab90, 0x880b8388, 
                   0x468cca46, 0xeec729ee, 0xb86bd3b8, 0x14283c14, 
                   0xdea779de, 0x5ebce25e, 0x0b161d0b, 0xdbad76db,
                   0xe0db3be0, 0x32645632, 0x3a744e3a, 0x0a141e0a, 
                   0x4992db49, 0x060c0a06, 0x24486c24, 0x5cb8e45c, 
                   0xc29f5dc2, 0xd3bd6ed3, 0xac43efac, 0x62c4a662,
                   0x9139a891, 0x9531a495, 0xe4d337e4, 0x79f28b79, 
                   0xe7d532e7, 0xc88b43c8, 0x376e5937, 0x6ddab76d, 
                   0x8d018c8d, 0xd5b164d5, 0x4e9cd24e, 0xa949e0a9,
                   0x6cd8b46c, 0x56acfa56, 0xf4f307f4, 0xeacf25ea, 
                   0x65caaf65, 0x7af48e7a, 0xae47e9ae, 0x08101808, 
                   0xba6fd5ba, 0x78f08878, 0x254a6f25, 0x2e5c722e,
                   0x1c38241c, 0xa657f1a6, 0xb473c7b4, 0xc69751c6, 
                   0xe8cb23e8, 0xdda17cdd, 0x74e89c74, 0x1f3e211f, 
                   0x4b96dd4b, 0xbd61dcbd, 0x8b0d868b, 0x8a0f858a,
                   0x70e09070, 0x3e7c423e, 0xb571c4b5, 0x66ccaa66, 
                   0x4890d848, 0x03060503, 0xf6f701f6, 0x0e1c120e, 
                   0x61c2a361, 0x356a5f35, 0x57aef957, 0xb969d0b9,
                   0x86179186, 0xc19958c1, 0x1d3a271d, 0x9e27b99e, 
                   0xe1d938e1, 0xf8eb13f8, 0x982bb398, 0x11223311, 
                   0x69d2bb69, 0xd9a970d9, 0x8e07898e, 0x9433a794,
                   0x9b2db69b, 0x1e3c221e, 0x87159287, 0xe9c920e9, 
                   0xce8749ce, 0x55aaff55, 0x28507828, 0xdfa57adf, 
                   0x8c038f8c, 0xa159f8a1, 0x89098089, 0x0d1a170d,
                   0xbf65dabf, 0xe6d731e6, 0x4284c642, 0x68d0b868, 
                   0x4182c341, 0x9929b099, 0x2d5a772d, 0x0f1e110f, 
                   0xb07bcbb0, 0x54a8fc54, 0xbb6dd6bb, 0x162c3a16};

  int table3[] = {0xc6a56363, 0xf8847c7c, 0xee997777, 0xf68d7b7b,
                   0xff0df2f2, 0xd6bd6b6b, 0xdeb16f6f, 0x9154c5c5, 
                   0x60503030, 0x02030101, 0xcea96767, 0x567d2b2b, 
                   0xe719fefe, 0xb562d7d7, 0x4de6abab, 0xec9a7676,
                   0x8f45caca, 0x1f9d8282, 0x8940c9c9, 0xfa877d7d, 
                   0xef15fafa, 0xb2eb5959, 0x8ec94747, 0xfb0bf0f0, 
                   0x41ecadad, 0xb367d4d4, 0x5ffda2a2, 0x45eaafaf,
                   0x23bf9c9c, 0x53f7a4a4, 0xe4967272, 0x9b5bc0c0, 
                   0x75c2b7b7, 0xe11cfdfd, 0x3dae9393, 0x4c6a2626, 
                   0x6c5a3636, 0x7e413f3f, 0xf502f7f7, 0x834fcccc,
                   0x685c3434, 0x51f4a5a5, 0xd134e5e5, 0xf908f1f1, 
                   0xe2937171, 0xab73d8d8, 0x62533131, 0x2a3f1515, 
                   0x080c0404, 0x9552c7c7, 0x46652323, 0x9d5ec3c3,
                   0x30281818, 0x37a19696, 0x0a0f0505, 0x2fb59a9a, 
                   0x0e090707, 0x24361212, 0x1b9b8080, 0xdf3de2e2, 
                   0xcd26ebeb, 0x4e692727, 0x7fcdb2b2, 0xea9f7575,
                   0x121b0909, 0x1d9e8383, 0x58742c2c, 0x342e1a1a, 
                   0x362d1b1b, 0xdcb26e6e, 0xb4ee5a5a, 0x5bfba0a0, 
                   0xa4f65252, 0x764d3b3b, 0xb761d6d6, 0x7dceb3b3,
                   0x527b2929, 0xdd3ee3e3, 0x5e712f2f, 0x13978484, 
                   0xa6f55353, 0xb968d1d1, 0x00000000, 0xc12ceded, 
                   0x40602020, 0xe31ffcfc, 0x79c8b1b1, 0xb6ed5b5b,
                   0xd4be6a6a, 0x8d46cbcb, 0x67d9bebe, 0x724b3939, 
                   0x94de4a4a, 0x98d44c4c, 0xb0e85858, 0x854acfcf, 
                   0xbb6bd0d0, 0xc52aefef, 0x4fe5aaaa, 0xed16fbfb,
                   0x86c54343, 0x9ad74d4d, 0x66553333, 0x11948585, 
                   0x8acf4545, 0xe910f9f9, 0x04060202, 0xfe817f7f, 
                   0xa0f05050, 0x78443c3c, 0x25ba9f9f, 0x4be3a8a8,
                   0xa2f35151, 0x5dfea3a3, 0x80c04040, 0x058a8f8f, 
                   0x3fad9292, 0x21bc9d9d, 0x70483838, 0xf104f5f5, 
                   0x63dfbcbc, 0x77c1b6b6, 0xaf75dada, 0x42632121,
                   0x20301010, 0xe51affff, 0xfd0ef3f3, 0xbf6dd2d2, 
                   0x814ccdcd, 0x18140c0c, 0x26351313, 0xc32fecec, 
                   0xbee15f5f, 0x35a29797, 0x88cc4444, 0x2e391717,
                   0x9357c4c4, 0x55f2a7a7, 0xfc827e7e, 0x7a473d3d, 
                   0xc8ac6464, 0xbae75d5d, 0x322b1919, 0xe6957373, 
                   0xc0a06060, 0x19988181, 0x9ed14f4f, 0xa37fdcdc,
                   0x44662222, 0x547e2a2a, 0x3bab9090, 0x0b838888, 
                   0x8cca4646, 0xc729eeee, 0x6bd3b8b8, 0x283c1414, 
                   0xa779dede, 0xbce25e5e, 0x161d0b0b, 0xad76dbdb,
                   0xdb3be0e0, 0x64563232, 0x744e3a3a, 0x141e0a0a, 
                   0x92db4949, 0x0c0a0606, 0x486c2424, 0xb8e45c5c, 
                   0x9f5dc2c2, 0xbd6ed3d3, 0x43efacac, 0xc4a66262,
                   0x39a89191, 0x31a49595, 0xd337e4e4, 0xf28b7979, 
                   0xd532e7e7, 0x8b43c8c8, 0x6e593737, 0xdab76d6d, 
                   0x018c8d8d, 0xb164d5d5, 0x9cd24e4e, 0x49e0a9a9,
                   0xd8b46c6c, 0xacfa5656, 0xf307f4f4, 0xcf25eaea, 
                   0xcaaf6565, 0xf48e7a7a, 0x47e9aeae, 0x10180808, 
                   0x6fd5baba, 0xf0887878, 0x4a6f2525, 0x5c722e2e,
                   0x38241c1c, 0x57f1a6a6, 0x73c7b4b4, 0x9751c6c6, 
                   0xcb23e8e8, 0xa17cdddd, 0xe89c7474, 0x3e211f1f, 
                   0x96dd4b4b, 0x61dcbdbd, 0x0d868b8b, 0x0f858a8a,
                   0xe0907070, 0x7c423e3e, 0x71c4b5b5, 0xccaa6666, 
                   0x90d84848, 0x06050303, 0xf701f6f6, 0x1c120e0e, 
                   0xc2a36161, 0x6a5f3535, 0xaef95757, 0x69d0b9b9,
                   0x17918686, 0x9958c1c1, 0x3a271d1d, 0x27b99e9e, 
                   0xd938e1e1, 0xeb13f8f8, 0x2bb39898, 0x22331111, 
                   0xd2bb6969, 0xa970d9d9, 0x07898e8e, 0x33a79494,
                   0x2db69b9b, 0x3c221e1e, 0x15928787, 0xc920e9e9, 
                   0x8749cece, 0xaaff5555, 0x50782828, 0xa57adfdf, 
                   0x038f8c8c, 0x59f8a1a1, 0x09808989, 0x1a170d0d,
                   0x65dabfbf, 0xd731e6e6, 0x84c64242, 0xd0b86868, 
                   0x82c34141, 0x29b09999, 0x5a772d2d, 0x1e110f0f, 
                   0x7bcbb0b0, 0xa8fc5454, 0x6dd6bbbb, 0x2c3a1616};

  short table4[] = {0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5,
                    0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76, 
                    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 
                    0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0, 
                    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 
                    0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15, 
                    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 
                    0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75, 
                    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 
                    0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84, 
                    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 
                    0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf, 
                    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 
                    0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8, 
                    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 
                    0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2, 
                    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 
                    0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73, 
                    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 
                    0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb, 
                    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 
                    0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79, 
                    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 
                    0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08, 
                    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 
                    0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a, 
                    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 
                    0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e, 
                    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 
                    0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf, 
                    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 
                    0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16};

   short s2 = grab8(s1_arr[3]);
   short s3 = grab8 (s1_arr[3] >>> 8);
   short s4 = grab8 (s1_arr[3] >>> 16);
   short s5 = grab8 (s1_arr[3] >>> 24);
   short s6 = grab8(s1_arr[2]);
   short s7 = grab8 (s1_arr[2] >>> 8);
   short s8 = grab8 (s1_arr[2] >>> 16);
   short s9 = grab8 (s1_arr[2] >>> 24);
   short s10 = grab8(s1_arr[1]);
   short s11 = grab8 (s1_arr[1] >>> 8);
   short s12 = grab8 (s1_arr[1] >>> 16);
   short s13 = grab8 (s1_arr[1] >>> 24);
   short s14 = grab8(s1_arr[0]);
   short s15 = grab8 (s1_arr[0] >>> 8);
   short s16 = grab8 (s1_arr[0] >>> 16);
   short s17 = grab8 (s1_arr[0] >>> 24);
   int  s18 = ((s16 << 8) | s17);
   int  s19 = ((s14 << 8) | s15);
   int  s20 = (s19 << 16) | s18;
   short s21 = grab8(s0_arr[3]);
   short s22 = grab8 (s0_arr[3] >>> 8);
   short s23 = grab8 (s0_arr[3] >>> 16);
   short s24 = grab8 (s0_arr[3] >>> 24);
   short s25 = grab8 (s0_arr[2]);
   short s26 = grab8 (s0_arr[2] >>> 8);
   short s27 = grab8 (s0_arr[2] >>> 16);
   short s28 = grab8 (s0_arr[2] >>> 24);
   short s29 = grab8 (s0_arr[1]);
   short s30 = grab8 (s0_arr[1] >>> 8);
   short s31 = grab8 (s0_arr[1] >>> 16);
   short s32 = grab8 (s0_arr[1] >>> 24);
   short s33 = grab8(s0_arr[0]);
   short s34 = grab8 (s0_arr[0] >>> 8);
   short s35 = grab8 (s0_arr[0] >>> 16);
   short s36 = grab8 (s0_arr[0] >>> 24);
   int  s37 = ((s35 << 8) | s36);
   int  s38 = ((s33 << 8) | s34);
   int  s39 = (s38 << 16) | s37;
   int  s40 = s20 ^ s39;
   short s41 = grab8(s40);
   short s42 = grab8 (s40 >>> 8);
   short s43 = grab8 (s40 >>> 16);
   short s44 = grab8 (s40 >>> 24);
   int  s45 = ((s12 << 8) | s13);
   int  s46 = ((s10 << 8) | s11);
   int  s47 = (s46 << 16) | s45;
   int  s48 = ((s31 << 8) | s32);
   int  s49 = ((s29 << 8) | s30);
   int  s50 = (s49 << 16) | s48;
   int  s51 = s47 ^ s50;
   short s52 = grab8(s51);
   short s53 = grab8 (s51 >>> 8);
   short s54 = grab8 (s51 >>> 16);
   short s55 = grab8 (s51 >>> 24);
   int  s56 = ((s8 << 8) | s9);
   int  s57 = ((s6 << 8) | s7);
   int  s58 = (s57 << 16) | s56;
   int  s59 = ((s27 << 8) | s28);
   int  s60 = ((s25 << 8) | s26);
   int  s61 = (s60 << 16) | s59;
   int  s62 = s58 ^ s61;
   short s63 = grab8(s62);
   short s64 = grab8 (s62 >>> 8);
   short s65 = grab8 (s62 >>> 16);
   short s66 = grab8 (s62 >>> 24);
   int  s67 = ((s4 << 8) | s5);
   int  s68 = ((s2 << 8) | s3);
   int  s69 = (s68 << 16) | s67;
   int  s70 = ((s23 << 8) | s24);
   int  s71 = ((s21 << 8) | s22);
   int  s72 = (s71 << 16) | s70;
   int  s73 = s69 ^ s72;
   short s74 = grab8(s73);
   short s75 = grab8 (s73 >>> 8);
   short s76 = grab8 (s73 >>> 16);
   short s77 = grab8 (s73 >>> 24);
   int  s78 = table0[s41];
   int  s79 = table1[s53];
   int  s80 = s78 ^ s79;
   int  s81 = table2[s65];
   int  s82 = s80 ^ s81;
   int  s83 = table3[s77];
   int  s84 = s82 ^ s83;
   int  s85 = (s72 >>> 8) | (s72 << 24);
   short s86 = grab8(s85);
   short s87 = grab8 (s85 >>> 8);
   short s88 = grab8 (s85 >>> 16);
   short s89 = grab8 (s85 >>> 24);
   short s90 = table4[s86];
   short s91 = table4[s87];
   int  s92 = ((s91 << 8) | s90);
   short s93 = table4[s88];
   short s94 = table4[s89];
   int  s95 = ((s94 << 8) | s93);
   int  s96 = (s95 << 16) | s92;
   int  s97 = s96 ^ 1;
   int  s98 = s39 ^ s97;
   int  s99 = s84 ^ s98;
   short s100 = grab8(s99);
   short s101 = grab8 (s99 >>> 8);
   short s102 = grab8 (s99 >>> 16);
   short s103 = grab8 (s99 >>> 24);
   int  s104 = table0[s52];
   int  s105 = table1[s64];
   int  s106 = s104 ^ s105;
   int  s107 = table2[s76];
   int  s108 = s106 ^ s107;
   int  s109 = table3[s44];
   int  s110 = s108 ^ s109;
   int  s111 = s50 ^ s98;
   int  s112 = s110 ^ s111;
   short s113 = grab8(s112);
   short s114 = grab8 (s112 >>> 8);
   short s115 = grab8 (s112 >>> 16);
   short s116 = grab8 (s112 >>> 24);
   int  s117 = table0[s63];
   int  s118 = table1[s75];
   int  s119 = s117 ^ s118;
   int  s120 = table2[s43];
   int  s121 = s119 ^ s120;
   int  s122 = table3[s55];
   int  s123 = s121 ^ s122;
   int  s124 = s61 ^ s111;
   int  s125 = s123 ^ s124;
   short s126 = grab8(s125);
   short s127 = grab8 (s125 >>> 8);
   short s128 = grab8 (s125 >>> 16);
   short s129 = grab8 (s125 >>> 24);
   int  s130 = table0[s74];
   int  s131 = table1[s42];
   int  s132 = s130 ^ s131;
   int  s133 = table2[s54];
   int  s134 = s132 ^ s133;
   int  s135 = table3[s66];
   int  s136 = s134 ^ s135;
   int  s137 = s72 ^ s124;
   int  s138 = s136 ^ s137;
   short s139 = grab8(s138);
   short s140 = grab8 (s138 >>> 8);
   short s141 = grab8 (s138 >>> 16);
   short s142 = grab8 (s138 >>> 24);
   int  s143 = table0[s100];
   int  s144 = table1[s114];
   int  s145 = s143 ^ s144;
   int  s146 = table2[s128];
   int  s147 = s145 ^ s146;
   int  s148 = table3[s142];
   int  s149 = s147 ^ s148;
   int  s150 = (s137 >>> 8) | (s137 << 24);
   short s151 = grab8(s150);
   short s152 = grab8 (s150 >>> 8);
   short s153 = grab8 (s150 >>> 16);
   short s154 = grab8 (s150 >>> 24);
   short s155 = table4[s151];
   short s156 = table4[s152];
   int  s157 = ((s156 << 8) | s155);
   short s158 = table4[s153];
   short s159 = table4[s154];
   int  s160 = ((s159 << 8) | s158);
   int  s161 = (s160 << 16) | s157;
   int  s162 = s161 ^ 2;
   int  s163 = s98 ^ s162;
   int  s164 = s149 ^ s163;
   short s165 = grab8(s164);
   short s166 = grab8 (s164 >>> 8);
   short s167 = grab8 (s164 >>> 16);
   short s168 = grab8 (s164 >>> 24);
   int  s169 = table0[s113];
   int  s170 = table1[s127];
   int  s171 = s169 ^ s170;
   int  s172 = table2[s141];
   int  s173 = s171 ^ s172;
   int  s174 = table3[s103];
   int  s175 = s173 ^ s174;
   int  s176 = s111 ^ s163;
   int  s177 = s175 ^ s176;
   short s178 = grab8(s177);
   short s179 = grab8 (s177 >>> 8);
   short s180 = grab8 (s177 >>> 16);
   short s181 = grab8 (s177 >>> 24);
   int  s182 = table0[s126];
   int  s183 = table1[s140];
   int  s184 = s182 ^ s183;
   int  s185 = table2[s102];
   int  s186 = s184 ^ s185;
   int  s187 = table3[s116];
   int  s188 = s186 ^ s187;
   int  s189 = s124 ^ s176;
   int  s190 = s188 ^ s189;
   short s191 = grab8(s190);
   short s192 = grab8 (s190 >>> 8);
   short s193 = grab8 (s190 >>> 16);
   short s194 = grab8 (s190 >>> 24);
   int  s195 = table0[s139];
   int  s196 = table1[s101];
   int  s197 = s195 ^ s196;
   int  s198 = table2[s115];
   int  s199 = s197 ^ s198;
   int  s200 = table3[s129];
   int  s201 = s199 ^ s200;
   int  s202 = s137 ^ s189;
   int  s203 = s201 ^ s202;
   short s204 = grab8(s203);
   short s205 = grab8 (s203 >>> 8);
   short s206 = grab8 (s203 >>> 16);
   short s207 = grab8 (s203 >>> 24);
   int  s208 = table0[s165];
   int  s209 = table1[s179];
   int  s210 = s208 ^ s209;
   int  s211 = table2[s193];
   int  s212 = s210 ^ s211;
   int  s213 = table3[s207];
   int  s214 = s212 ^ s213;
   int  s215 = (s202 >>> 8) | (s202 << 24);
   short s216 = grab8(s215);
   short s217 = grab8 (s215 >>> 8);
   short s218 = grab8 (s215 >>> 16);
   short s219 = grab8 (s215 >>> 24);
   short s220 = table4[s216];
   short s221 = table4[s217];
   int  s222 = ((s221 << 8) | s220);
   short s223 = table4[s218];
   short s224 = table4[s219];
   int  s225 = ((s224 << 8) | s223);
   int  s226 = (s225 << 16) | s222;
   int  s227 = s226 ^ 4;
   int  s228 = s163 ^ s227;
   int  s229 = s214 ^ s228;
   short s230 = grab8(s229);
   short s231 = grab8 (s229 >>> 8);
   short s232 = grab8 (s229 >>> 16);
   short s233 = grab8 (s229 >>> 24);
   int  s234 = table0[s178];
   int  s235 = table1[s192];
   int  s236 = s234 ^ s235;
   int  s237 = table2[s206];
   int  s238 = s236 ^ s237;
   int  s239 = table3[s168];
   int  s240 = s238 ^ s239;
   int  s241 = s176 ^ s228;
   int  s242 = s240 ^ s241;
   short s243 = grab8(s242);
   short s244 = grab8 (s242 >>> 8);
   short s245 = grab8 (s242 >>> 16);
   short s246 = grab8 (s242 >>> 24);
   int  s247 = table0[s191];
   int  s248 = table1[s205];
   int  s249 = s247 ^ s248;
   int  s250 = table2[s167];
   int  s251 = s249 ^ s250;
   int  s252 = table3[s181];
   int  s253 = s251 ^ s252;
   int  s254 = s189 ^ s241;
   int  s255 = s253 ^ s254;
   short s256 = grab8(s255);
   short s257 = grab8 (s255 >>> 8);
   short s258 = grab8 (s255 >>> 16);
   short s259 = grab8 (s255 >>> 24);
   int  s260 = table0[s204];
   int  s261 = table1[s166];
   int  s262 = s260 ^ s261;
   int  s263 = table2[s180];
   int  s264 = s262 ^ s263;
   int  s265 = table3[s194];
   int  s266 = s264 ^ s265;
   int  s267 = s202 ^ s254;
   int  s268 = s266 ^ s267;
   short s269 = grab8(s268);
   short s270 = grab8 (s268 >>> 8);
   short s271 = grab8 (s268 >>> 16);
   short s272 = grab8 (s268 >>> 24);
   int  s273 = table0[s230];
   int  s274 = table1[s244];
   int  s275 = s273 ^ s274;
   int  s276 = table2[s258];
   int  s277 = s275 ^ s276;
   int  s278 = table3[s272];
   int  s279 = s277 ^ s278;
   int  s280 = (s267 >>> 8) | (s267 << 24);
   short s281 = grab8(s280);
   short s282 = grab8 (s280 >>> 8);
   short s283 = grab8 (s280 >>> 16);
   short s284 = grab8 (s280 >>> 24);
   short s285 = table4[s281];
   short s286 = table4[s282];
   int  s287 = ((s286 << 8) | s285);
   short s288 = table4[s283];
   short s289 = table4[s284];
   int  s290 = ((s289 << 8) | s288);
   int  s291 = (s290 << 16) | s287;
   int  s292 = s291 ^ 8;
   int  s293 = s228 ^ s292;
   int  s294 = s279 ^ s293;
   short s295 = grab8(s294);
   short s296 = grab8 (s294 >>> 8);
   short s297 = grab8 (s294 >>> 16);
   short s298 = grab8 (s294 >>> 24);
   int  s299 = table0[s243];
   int  s300 = table1[s257];
   int  s301 = s299 ^ s300;
   int  s302 = table2[s271];
   int  s303 = s301 ^ s302;
   int  s304 = table3[s233];
   int  s305 = s303 ^ s304;
   int  s306 = s241 ^ s293;
   int  s307 = s305 ^ s306;
   short s308 = grab8(s307);
   short s309 = grab8 (s307 >>> 8);
   short s310 = grab8 (s307 >>> 16);
   short s311 = grab8 (s307 >>> 24);
   int  s312 = table0[s256];
   int  s313 = table1[s270];
   int  s314 = s312 ^ s313;
   int  s315 = table2[s232];
   int  s316 = s314 ^ s315;
   int  s317 = table3[s246];
   int  s318 = s316 ^ s317;
   int  s319 = s254 ^ s306;
   int  s320 = s318 ^ s319;
   short s321 = grab8(s320);
   short s322 = grab8 (s320 >>> 8);
   short s323 = grab8 (s320 >>> 16);
   short s324 = grab8 (s320 >>> 24);
   int  s325 = table0[s269];
   int  s326 = table1[s231];
   int  s327 = s325 ^ s326;
   int  s328 = table2[s245];
   int  s329 = s327 ^ s328;
   int  s330 = table3[s259];
   int  s331 = s329 ^ s330;
   int  s332 = s267 ^ s319;
   int  s333 = s331 ^ s332;
   short s334 = grab8(s333);
   short s335 = grab8 (s333 >>> 8);
   short s336 = grab8 (s333 >>> 16);
   short s337 = grab8 (s333 >>> 24);
   int  s338 = table0[s295];
   int  s339 = table1[s309];
   int  s340 = s338 ^ s339;
   int  s341 = table2[s323];
   int  s342 = s340 ^ s341;
   int  s343 = table3[s337];
   int  s344 = s342 ^ s343;
   int  s345 = (s332 >>> 8) | (s332 << 24);
   short s346 = grab8(s345);
   short s347 = grab8 (s345 >>> 8);
   short s348 = grab8 (s345 >>> 16);
   short s349 = grab8 (s345 >>> 24);
   short s350 = table4[s346];
   short s351 = table4[s347];
   int  s352 = ((s351 << 8) | s350);
   short s353 = table4[s348];
   short s354 = table4[s349];
   int  s355 = ((s354 << 8) | s353);
   int  s356 = (s355 << 16) | s352;
   int  s357 = s356 ^ 16;
   int  s358 = s293 ^ s357;
   int  s359 = s344 ^ s358;
   short s360 = grab8(s359);
   short s361 = grab8 (s359 >>> 8);
   short s362 = grab8 (s359 >>> 16);
   short s363 = grab8 (s359 >>> 24);
   int  s364 = table0[s308];
   int  s365 = table1[s322];
   int  s366 = s364 ^ s365;
   int  s367 = table2[s336];
   int  s368 = s366 ^ s367;
   int  s369 = table3[s298];
   int  s370 = s368 ^ s369;
   int  s371 = s306 ^ s358;
   int  s372 = s370 ^ s371;
   short s373 = grab8(s372);
   short s374 = grab8 (s372 >>> 8);
   short s375 = grab8 (s372 >>> 16);
   short s376 = grab8 (s372 >>> 24);
   int  s377 = table0[s321];
   int  s378 = table1[s335];
   int  s379 = s377 ^ s378;
   int  s380 = table2[s297];
   int  s381 = s379 ^ s380;
   int  s382 = table3[s311];
   int  s383 = s381 ^ s382;
   int  s384 = s319 ^ s371;
   int  s385 = s383 ^ s384;
   short s386 = grab8(s385);
   short s387 = grab8 (s385 >>> 8);
   short s388 = grab8 (s385 >>> 16);
   short s389 = grab8 (s385 >>> 24);
   int  s390 = table0[s334];
   int  s391 = table1[s296];
   int  s392 = s390 ^ s391;
   int  s393 = table2[s310];
   int  s394 = s392 ^ s393;
   int  s395 = table3[s324];
   int  s396 = s394 ^ s395;
   int  s397 = s332 ^ s384;
   int  s398 = s396 ^ s397;
   short s399 = grab8(s398);
   short s400 = grab8 (s398 >>> 8);
   short s401 = grab8 (s398 >>> 16);
   short s402 = grab8 (s398 >>> 24);
   int  s403 = table0[s360];
   int  s404 = table1[s374];
   int  s405 = s403 ^ s404;
   int  s406 = table2[s388];
   int  s407 = s405 ^ s406;
   int  s408 = table3[s402];
   int  s409 = s407 ^ s408;
   int  s410 = (s397 >>> 8) | (s397 << 24);
   short s411 = grab8(s410);
   short s412 = grab8(s410 >>> 8);
   short s413 = grab8(s410 >>> 16);
   short s414 = grab8(s410 >>> 24);
   short s415 = table4[s411];
   short s416 = table4[s412];
   int  s417 = ((s416 << 8) | s415);
   short s418 = table4[s413];
   short s419 = table4[s414];
   int  s420 = ((s419 << 8) | s418);
   int  s421 = (s420 << 16) | s417;
   int  s422 = s421 ^ 32;
   int  s423 = s358 ^ s422;
   int  s424 = s409 ^ s423;
   short s425 = grab8(s424);
   short s426 = grab8(s424 >>> 8);
   short s427 = grab8(s424 >>> 16);
   short s428 = grab8(s424 >>> 24);
   int  s429 = table0[s373];
   int  s430 = table1[s387];
   int  s431 = s429 ^ s430;
   int  s432 = table2[s401];
   int  s433 = s431 ^ s432;
   int  s434 = table3[s363];
   int  s435 = s433 ^ s434;
   int  s436 = s371 ^ s423;
   int  s437 = s435 ^ s436;
   short s438 = grab8(s437);
   short s439 = grab8(s437 >>> 8);
   short s440 = grab8(s437 >>> 16);
   short s441 = grab8(s437 >>> 24);
   int  s442 = table0[s386];
   int  s443 = table1[s400];
   int  s444 = s442 ^ s443;
   int  s445 = table2[s362];
   int  s446 = s444 ^ s445;
   int  s447 = table3[s376];
   int  s448 = s446 ^ s447;
   int  s449 = s384 ^ s436;
   int  s450 = s448 ^ s449;
   short s451 = grab8(s450);
   short s452 = grab8(s450 >>> 8);
   short s453 = grab8(s450 >>> 16);
   short s454 = grab8(s450 >>> 24);
   int  s455 = table0[s399];
   int  s456 = table1[s361];
   int  s457 = s455 ^ s456;
   int  s458 = table2[s375];
   int  s459 = s457 ^ s458;
   int  s460 = table3[s389];
   int  s461 = s459 ^ s460;
   int  s462 = s397 ^ s449;
   int  s463 = s461 ^ s462;
   short s464 = grab8(s463);
   short s465 = grab8(s463 >>> 8);
   short s466 = grab8(s463 >>> 16);
   short s467 = grab8(s463 >>> 24);
   int  s468 = table0[s425];
   int  s469 = table1[s439];
   int  s470 = s468 ^ s469;
   int  s471 = table2[s453];
   int  s472 = s470 ^ s471;
   int  s473 = table3[s467];
   int  s474 = s472 ^ s473;
   int  s475 = (s462 >>> 8) | (s462 << 24);
   short s476 = grab8(s475);
   short s477 = grab8(s475 >>> 8);
   short s478 = grab8(s475 >>> 16);
   short s479 = grab8(s475 >>> 24);
   short s480 = table4[s476];
   short s481 = table4[s477];
   int  s482 = ((s481 << 8) | s480);
   short s483 = table4[s478];
   short s484 = table4[s479];
   int  s485 = ((s484 << 8) | s483);
   int  s486 = (s485 << 16) | s482;
   int  s487 = s486 ^ 64;
   int  s488 = s423 ^ s487;
   int  s489 = s474 ^ s488;
   short s490 = grab8(s489);
   short s491 = grab8(s489 >>> 8);
   short s492 = grab8(s489 >>> 16);
   short s493 = grab8(s489 >>> 24);
   int  s494 = table0[s438];
   int  s495 = table1[s452];
   int  s496 = s494 ^ s495;
   int  s497 = table2[s466];
   int  s498 = s496 ^ s497;
   int  s499 = table3[s428];
   int  s500 = s498 ^ s499;
   int  s501 = s436 ^ s488;
   int  s502 = s500 ^ s501;
   short s503 = grab8(s502);
   short s504 = grab8(s502 >>> 8);
   short s505 = grab8(s502 >>> 16);
   short s506 = grab8(s502 >>> 24);
   int  s507 = table0[s451];
   int  s508 = table1[s465];
   int  s509 = s507 ^ s508;
   int  s510 = table2[s427];
   int  s511 = s509 ^ s510;
   int  s512 = table3[s441];
   int  s513 = s511 ^ s512;
   int  s514 = s449 ^ s501;
   int  s515 = s513 ^ s514;
   short s516 = grab8(s515);
   short s517 = grab8(s515 >>> 8);
   short s518 = grab8(s515 >>> 16);
   short s519 = grab8(s515 >>> 24);
   int  s520 = table0[s464];
   int  s521 = table1[s426];
   int  s522 = s520 ^ s521;
   int  s523 = table2[s440];
   int  s524 = s522 ^ s523;
   int  s525 = table3[s454];
   int  s526 = s524 ^ s525;
   int  s527 = s462 ^ s514;
   int  s528 = s526 ^ s527;
   short s529 = grab8(s528);
   short s530 = grab8(s528 >>> 8);
   short s531 = grab8(s528 >>> 16);
   short s532 = grab8(s528 >>> 24);
   int  s533 = table0[s490];
   int  s534 = table1[s504];
   int  s535 = s533 ^ s534;
   int  s536 = table2[s518];
   int  s537 = s535 ^ s536;
   int  s538 = table3[s532];
   int  s539 = s537 ^ s538;
   int  s540 = (s527 >>> 8) | (s527 << 24);
   short s541 = grab8(s540);
   short s542 = grab8(s540 >>> 8);
   short s543 = grab8(s540 >>> 16);
   short s544 = grab8(s540 >>> 24);
   short s545 = table4[s541];
   short s546 = table4[s542];
   int  s547 = ((s546 << 8) | s545);
   short s548 = table4[s543];
   short s549 = table4[s544];
   int  s550 = ((s549 << 8) | s548);
   int  s551 = (s550 << 16) | s547;
   int  s552 = s551 ^ 128;
   int  s553 = s488 ^ s552;
   int  s554 = s539 ^ s553;
   short s555 = grab8(s554);
   short s556 = grab8(s554 >>> 8);
   short s557 = grab8(s554 >>> 16);
   short s558 = grab8(s554 >>> 24);
   int  s559 = table0[s503];
   int  s560 = table1[s517];
   int  s561 = s559 ^ s560;
   int  s562 = table2[s531];
   int  s563 = s561 ^ s562;
   int  s564 = table3[s493];
   int  s565 = s563 ^ s564;
   int  s566 = s501 ^ s553;
   int  s567 = s565 ^ s566;
   short s568 = grab8(s567);
   short s569 = grab8(s567 >>> 8);
   short s570 = grab8(s567 >>> 16);
   short s571 = grab8(s567 >>> 24);
   int  s572 = table0[s516];
   int  s573 = table1[s530];
   int  s574 = s572 ^ s573;
   int  s575 = table2[s492];
   int  s576 = s574 ^ s575;
   int  s577 = table3[s506];
   int  s578 = s576 ^ s577;
   int  s579 = s514 ^ s566;
   int  s580 = s578 ^ s579;
   short s581 = grab8(s580);
   short s582 = grab8(s580 >>> 8);
   short s583 = grab8(s580 >>> 16);
   short s584 = grab8(s580 >>> 24);
   int  s585 = table0[s529];
   int  s586 = table1[s491];
   int  s587 = s585 ^ s586;
   int  s588 = table2[s505];
   int  s589 = s587 ^ s588;
   int  s590 = table3[s519];
   int  s591 = s589 ^ s590;
   int  s592 = s527 ^ s579;
   int  s593 = s591 ^ s592;
   short s594 = grab8(s593);
   short s595 = grab8(s593 >>> 8);
   short s596 = grab8(s593 >>> 16);
   short s597 = grab8(s593 >>> 24);
   int  s598 = table0[s555];
   int  s599 = table1[s569];
   int  s600 = s598 ^ s599;
   int  s601 = table2[s583];
   int  s602 = s600 ^ s601;
   int  s603 = table3[s597];
   int  s604 = s602 ^ s603;
   int  s605 = (s592 >>> 8) | (s592 << 24);
   short s606 = grab8(s605);
   short s607 = grab8(s605 >>> 8);
   short s608 = grab8(s605 >>> 16);
   short s609 = grab8(s605 >>> 24);
   short s610 = table4[s606];
   short s611 = table4[s607];
   int  s612 = ((s611 << 8) | s610);
   short s613 = table4[s608];
   short s614 = table4[s609];
   int  s615 = ((s614 << 8) | s613);
   int  s616 = (s615 << 16) | s612;
   int  s617 = s616 ^ 27;
   int  s618 = s553 ^ s617;
   int  s619 = s604 ^ s618;
   short s620 = grab8(s619);
   short s621 = grab8(s619 >>> 8);
   short s622 = grab8(s619 >>> 16);
   short s623 = grab8(s619 >>> 24);
   int  s624 = table0[s568];
   int  s625 = table1[s582];
   int  s626 = s624 ^ s625;
   int  s627 = table2[s596];
   int  s628 = s626 ^ s627;
   int  s629 = table3[s558];
   int  s630 = s628 ^ s629;
   int  s631 = s566 ^ s618;
   int  s632 = s630 ^ s631;
   short s633 = grab8(s632);
   short s634 = grab8(s632 >>> 8);
   short s635 = grab8(s632 >>> 16);
   short s636 = grab8(s632 >>> 24);
   int  s637 = table0[s581];
   int  s638 = table1[s595];
   int  s639 = s637 ^ s638;
   int  s640 = table2[s557];
   int  s641 = s639 ^ s640;
   int  s642 = table3[s571];
   int  s643 = s641 ^ s642;
   int  s644 = s579 ^ s631;
   int  s645 = s643 ^ s644;
   short s646 = grab8(s645);
   short s647 = grab8(s645 >>> 8);
   short s648 = grab8(s645 >>> 16);
   short s649 = grab8(s645 >>> 24);
   int  s650 = table0[s594];
   int  s651 = table1[s556];
   //System.out.printf("s556 %d\n", s556);
   //System.out.printf("s651 %d\n", s651);
   //System.out.printf("s651 %d\n", 0xFFFF & s651);
   //System.out.printf("s651 %d\n", (((long)2) << 32) + (long) s651);
   int  s652 = s650 ^ s651;
   int  s653 = table2[s570];
   int  s654 = s652 ^ s653;
   int  s655 = table3[s584];
   int  s656 = s654 ^ s655;
   int  s657 = s592 ^ s644;
   int  s658 = s656 ^ s657;
   short s659 = grab8(s658);
   short s660 = grab8(s658 >>> 8);
   short s661 = grab8(s658 >>> 16);
   short s662 = grab8(s658 >>> 24);
   short s663 = table4[s620];
   int  s664 = s663;
   short s665 = table4[s634];
   int  s666 = s665;
   int  s667 = s666 << 8;
   int  s668 = s664 ^ s667;
   short s669 = table4[s648];
   int  s670 = s669;
   int  s671 = s670 << 16;
   int  s672 = s668 ^ s671;
   short s673 = table4[s662];
   int  s674 = s673;
   int  s675 = s674 << 24;
   int  s676 = s672 ^ s675;
   int  s677 = (s657 >>> 8) | (s657 << 24);
   short s678 = grab8(s677);
   short s679 = grab8(s677 >>> 8);
   short s680 = grab8(s677 >>> 16);
   short s681 = grab8(s677 >>> 24);
   short s682 = table4[s678];
   short s683 = table4[s679];
   int  s684 = ((s683 << 8) | s682);
   short s685 = table4[s680];
   short s686 = table4[s681];
   int  s687 = ((s686 << 8) | s685);
   int  s688 = (s687 << 16) | s684;
   int  s689 = s688 ^ 54;
   int  s690 = s618 ^ s689;
   int  s691 = s676 ^ s690;
   short s692 = grab8(s691);
   short s693 = grab8(s691 >>> 8);
   short s694 = grab8(s691 >>> 16);
   short s695 = grab8(s691 >>> 24);
   short s696 = table4[s633];
   int  s697 = s696;
   short s698 = table4[s647];
   int  s699 = s698;
   int  s700 = s699 << 8;
   int  s701 = s697 ^ s700;
   short s702 = table4[s661];
   int  s703 = s702;
   int  s704 = s703 << 16;
   int  s705 = s701 ^ s704;
   short s706 = table4[s623];
   int  s707 = s706;
   int  s708 = s707 << 24;
   int  s709 = s705 ^ s708;
   int  s710 = s631 ^ s690;
   int  s711 = s709 ^ s710;
   short s712 = grab8(s711);
   short s713 = grab8(s711 >>> 8);
   short s714 = grab8(s711 >>> 16);
   short s715 = grab8(s711 >>> 24);
   short s716 = table4[s646];
   int  s717 = s716;
   short s718 = table4[s660];
   int  s719 = s718;
   int  s720 = s719 << 8;
   int  s721 = s717 ^ s720;
   short s722 = table4[s622];
   int  s723 = s722;
   int  s724 = s723 << 16;
   int  s725 = s721 ^ s724;
   short s726 = table4[s636];
   int  s727 = s726;
   int  s728 = s727 << 24;
   int  s729 = s725 ^ s728;
   int  s730 = s644 ^ s710;
   int  s731 = s729 ^ s730;
   short s732 = grab8(s731);
   short s733 = grab8(s731 >>> 8);
   short s734 = grab8(s731 >>> 16);
   short s735 = grab8(s731 >>> 24);
   short s736 = table4[s659];
   int  s737 = s736;
   short s738 = table4[s621];
   int  s739 = s738;
   int  s740 = s739 << 8;
   int  s741 = s737 ^ s740;
   short s742 = table4[s635];
   int  s743 = s742;
   int  s744 = s743 << 16;
   int  s745 = s741 ^ s744;
   short s746 = table4[s649];
   int  s747 = s746;
   int  s748 = s747 << 24;
   int  s749 = s745 ^ s748;
   int  s750 = s657 ^ s730;
   int  s751 = s749 ^ s750;
   short s752 = grab8(s751);
   short s753 = grab8(s751 >>> 8);
   short s754 = grab8(s751 >>> 16);
   short s755 = grab8(s751 >>> 24);
   int  s756 = ((s754 << 8) | s755);
   int  s757 = ((s752 << 8) | s753);
   int  s758 = (s757 << 16) | s756;
   int  s759 = ((s734 << 8) | s735);
   int  s760 = ((s732 << 8) | s733);
   int  s761 = (s760 << 16) | s759;
   int  s763 = ((s714 << 8) | s715);
   int  s764 = ((s712 << 8) | s713);
   int  s765 = (s764 << 16) | s763;
   int  s766 = ((s694 << 8) | s695);
   int  s767 = ((s692 << 8) | s693);
   int  s768 = (s767 << 16) | s766;
   
   int[] out0_arr = new int[] {s768, s765, s761, s758};
   return out0_arr;
 }
}

public class Main {
  public static void main(String[] args) {
    int[] key        = { 0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f };
    int[] pt         = { 0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff };
    int[] ctExpected = { 0x69c4e0d8, 0x6a7b0430, 0xd8cdb780, 0x70b4c55a };
    int[] ctGot      = AES128Encrypt.encrypt(key, pt);
    System.out.print("Key        : 0x");
    for(int i = 0; i < 4; ++i) System.out.printf("%08x", key[i]);
    System.out.print("\nPT         : 0x");
    for(int i = 0; i < 4; ++i) System.out.printf("%08x", pt[i]);
    System.out.print("\nCT-Expected: 0x");
    for(int i = 0; i < 4; ++i) System.out.printf("%08x", ctExpected[i]);
    System.out.print("\nCT-Got     : 0x");
    for(int i = 0; i < 4; ++i) System.out.printf("%08x", ctGot[i]);
    boolean ok = true;
    for(int i = 0; i < 4; ++i) if(ctGot[i] != ctExpected[i]) ok = false;
    if(ok) System.out.print("\nPASSED!\n"); else System.out.print("\nFAILED!\n");
  }
}
