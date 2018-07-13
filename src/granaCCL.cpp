// #include <Rcpp.h>
// using namespace Rcpp;
//
//
//
// void firstScanBBDT_OPT(const Mat1b &img, Mat1i& imgLabels, uint* P, uint &lunique) {
//   int width(img.cols), height(img.rows);
//
//   for (int r = 0; r<height; r += 2) {
//     // Get rows pointer
//     const uchar* const img_row = img.ptr<uchar>(r);
//     const uchar* const img_row_prev = (uchar *)(((char *)img_row) - img.step.p[0]);
//     const uchar* const img_row_prev_prev = (uchar *)(((char *)img_row_prev) - img.step.p[0]);
//     const uchar* const img_row_fol = (uchar *)(((char *)img_row) + img.step.p[0]);
//     uint* const imgLabels_row = imgLabels.ptr<uint>(r);
//     uint* const imgLabels_row_prev_prev = (uint *)(((char *)imgLabels_row) - imgLabels.step.p[0] - imgLabels.step.p[0]);
//     for (int c = 0; c < width; c += 2) {
//
//       // We work with 2x2 blocks
//       // +-+-+-+
//       // |P|Q|R|
//       // +-+-+-+
//       // |S|X|
//       // +-+-+
//
//       // The pixels are named as follows
//       // +---+---+---+
//       // |a b|c d|e f|
//       // |g h|i j|k l|
//       // +---+---+---+
//       // |m n|o p|
//       // |q r|s t|
//       // +---+---+
//
//       // Pixels a, f, l, q are not needed, since we need to understand the
//       // the connectivity between these blocks and those pixels only metter
//       // when considering the outer connectivities
//
//       // A bunch of defines used to check if the pixels are foreground,
//       // without going outside the image limits.
//
// #define condition_b c-1>=0 && r-2>=0 && img_row_prev_prev[c-1]>0
// #define condition_c r-2>=0 && img_row_prev_prev[c]>0
// #define condition_d c+1<width && r-2>=0 && img_row_prev_prev[c+1]>0
// #define condition_e c+2<width && r-2>=0 && img_row_prev_prev[c+2]>0
//
// #define condition_g c-2>=0 && r-1>=0 && img_row_prev[c-2]>0
// #define condition_h c-1>=0 && r-1>=0 && img_row_prev[c-1]>0
// #define condition_i r-1>=0 && img_row_prev[c]>0
// #define condition_j c+1<width && r-1>=0 && img_row_prev[c+1]>0
// #define condition_k c+2<width && r-1>=0 && img_row_prev[c+2]>0
//
// #define condition_m c-2>=0 && img_row[c-2]>0
// #define condition_n c-1>=0 && img_row[c-1]>0
// #define condition_o img_row[c]>0
// #define condition_p c+1<width && img_row[c+1]>0
//
// #define condition_r c-1>=0 && r+1<height && img_row_fol[c-1]>0
// #define condition_s r+1<height && img_row_fol[c]>0
// #define condition_t c+1<width && r+1<height && img_row_fol[c+1]>0
//
//       // This is a decision tree which allows to choose which action to
//       // perform, checking as few conditions as possible.
//       // Actions are available after the tree.
//
//       if (condition_o) {
//         if (condition_n) {
//           if (condition_j) {
//             if (condition_i) {
//               //Action_6: Assign label of block S
//               imgLabels_row[c] = imgLabels_row[c - 2];
//               continue;
//             }
//             else {
//               if (condition_c) {
//                 if (condition_h) {
//                   //Action_6: Assign label of block S
//                   imgLabels_row[c] = imgLabels_row[c - 2];
//                   continue;
//                 }
//                 else {
//                   if (condition_g) {
//                     if (condition_b) {
//                       //Action_6: Assign label of block S
//                       imgLabels_row[c] = imgLabels_row[c - 2];
//                       continue;
//                     }
//                     else {
//                       //Action_11: Merge labels of block Q and S
//                       imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                       continue;
//                     }
//                   }
//                   else {
//                     //Action_11: Merge labels of block Q and S
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                     continue;
//                   }
//                 }
//               }
//               else {
//                 //Action_11: Merge labels of block Q and S
//                 imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                 continue;
//               }
//             }
//           }
//           else {
//             if (condition_p) {
//               if (condition_k) {
//                 if (condition_d) {
//                   if (condition_i) {
//                     //Action_6: Assign label of block S
//                     imgLabels_row[c] = imgLabels_row[c - 2];
//                     continue;
//                   }
//                   else {
//                     if (condition_c) {
//                       if (condition_h) {
//                         //Action_6: Assign label of block S
//                         imgLabels_row[c] = imgLabels_row[c - 2];
//                         continue;
//                       }
//                       else {
//                         if (condition_g) {
//                           if (condition_b) {
//                             //Action_6: Assign label of block S
//                             imgLabels_row[c] = imgLabels_row[c - 2];
//                             continue;
//                           }
//                           else {
//                             //Action_12: Merge labels of block R and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                         else {
//                           //Action_12: Merge labels of block R and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                     }
//                     else {
//                       //Action_12: Merge labels of block R and S
//                       imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                       continue;
//                     }
//                   }
//                 }
//                 else {
//                   //Action_12: Merge labels of block R and S
//                   imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                   continue;
//                 }
//               }
//               else {
//                 //Action_6: Assign label of block S
//                 imgLabels_row[c] = imgLabels_row[c - 2];
//                 continue;
//               }
//             }
//             else {
//               //Action_6: Assign label of block S
//               imgLabels_row[c] = imgLabels_row[c - 2];
//               continue;
//             }
//           }
//         }
//         else {
//           if (condition_r) {
//             if (condition_j) {
//               if (condition_m) {
//                 if (condition_h) {
//                   if (condition_i) {
//                     //Action_6: Assign label of block S
//                     imgLabels_row[c] = imgLabels_row[c - 2];
//                     continue;
//                   }
//                   else {
//                     if (condition_c) {
//                       //Action_6: Assign label of block S
//                       imgLabels_row[c] = imgLabels_row[c - 2];
//                       continue;
//                     }
//                     else {
//                       //Action_11: Merge labels of block Q and S
//                       imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                       continue;
//                     }
//                   }
//                 }
//                 else {
//                   if (condition_g) {
//                     if (condition_b) {
//                       if (condition_i) {
//                         //Action_6: Assign label of block S
//                         imgLabels_row[c] = imgLabels_row[c - 2];
//                         continue;
//                       }
//                       else {
//                         if (condition_c) {
//                           //Action_6: Assign label of block S
//                           imgLabels_row[c] = imgLabels_row[c - 2];
//                           continue;
//                         }
//                         else {
//                           //Action_11: Merge labels of block Q and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                     }
//                     else {
//                       //Action_11: Merge labels of block Q and S
//                       imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                       continue;
//                     }
//                   }
//                   else {
//                     //Action_11: Merge labels of block Q and S
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                     continue;
//                   }
//                 }
//               }
//               else {
//                 if (condition_i) {
//                   //Action_11: Merge labels of block Q and S
//                   imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                   continue;
//                 }
//                 else {
//                   if (condition_h) {
//                     if (condition_c) {
//                       //Action_11: Merge labels of block Q and S
//                       imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                       continue;
//                     }
//                     else {
//                       //Action_14: Merge labels of block P, Q and S
//                       imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c - 2], imgLabels_row_prev_prev[c]), imgLabels_row[c - 2]);
//                       continue;
//                     }
//                   }
//                   else {
//                     //Action_11: Merge labels of block Q and S
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                     continue;
//                   }
//                 }
//               }
//             }
//             else {
//               if (condition_p) {
//                 if (condition_k) {
//                   if (condition_m) {
//                     if (condition_h) {
//                       if (condition_d) {
//                         if (condition_i) {
//                           //Action_6: Assign label of block S
//                           imgLabels_row[c] = imgLabels_row[c - 2];
//                           continue;
//                         }
//                         else {
//                           if (condition_c) {
//                             //Action_6: Assign label of block S
//                             imgLabels_row[c] = imgLabels_row[c - 2];
//                             continue;
//                           }
//                           else {
//                             //Action_12: Merge labels of block R and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                       }
//                       else {
//                         //Action_12: Merge labels of block R and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                     else {
//                       if (condition_d) {
//                         if (condition_g) {
//                           if (condition_b) {
//                             if (condition_i) {
//                               //Action_6: Assign label of block S
//                               imgLabels_row[c] = imgLabels_row[c - 2];
//                               continue;
//                             }
//                             else {
//                               if (condition_c) {
//                                 //Action_6: Assign label of block S
//                                 imgLabels_row[c] = imgLabels_row[c - 2];
//                                 continue;
//                               }
//                               else {
//                                 //Action_12: Merge labels of block R and S
//                                 imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                                 continue;
//                               }
//                             }
//                           }
//                           else {
//                             //Action_12: Merge labels of block R and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                         else {
//                           //Action_12: Merge labels of block R and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                       else {
//                         if (condition_i) {
//                           if (condition_g) {
//                             if (condition_b) {
//                               //Action_12: Merge labels of block R and S
//                               imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                               continue;
//                             }
//                             else {
//                               //Action_16: labels of block Q, R and S
//                               imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]), imgLabels_row[c - 2]);
//                               continue;
//                             }
//                           }
//                           else {
//                             //Action_16: labels of block Q, R and S
//                             imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]), imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                         else {
//                           //Action_12: Merge labels of block R and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                     }
//                   }
//                   else {
//                     if (condition_i) {
//                       if (condition_d) {
//                         //Action_12: Merge labels of block R and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                       else {
//                         //Action_16: labels of block Q, R and S
//                         imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]), imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                     else {
//                       if (condition_h) {
//                         if (condition_d) {
//                           if (condition_c) {
//                             //Action_12: Merge labels of block R and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                           else {
//                             //Action_15: Merge labels of block P, R and S
//                             imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c - 2], imgLabels_row_prev_prev[c + 2]), imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                         else {
//                           //Action_15: Merge labels of block P, R and S
//                           imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c - 2], imgLabels_row_prev_prev[c + 2]), imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                       else {
//                         //Action_12: Merge labels of block R and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                   }
//                 }
//                 else {
//                   if (condition_h) {
//                     if (condition_m) {
//                       //Action_6: Assign label of block S
//                       imgLabels_row[c] = imgLabels_row[c - 2];
//                       continue;
//                     }
//                     else {
//                       // ACTION_9 Merge labels of block P and S
//                       imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c - 2], imgLabels_row[c - 2]);
//                       continue;
//                     }
//                   }
//                   else {
//                     if (condition_i) {
//                       if (condition_m) {
//                         if (condition_g) {
//                           if (condition_b) {
//                             //Action_6: Assign label of block S
//                             imgLabels_row[c] = imgLabels_row[c - 2];
//                             continue;
//                           }
//                           else {
//                             //Action_11: Merge labels of block Q and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                         else {
//                           //Action_11: Merge labels of block Q and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                       else {
//                         //Action_11: Merge labels of block Q and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                     else {
//                       //Action_6: Assign label of block S
//                       imgLabels_row[c] = imgLabels_row[c - 2];
//                       continue;
//                     }
//                   }
//                 }
//               }
//               else {
//                 if (condition_h) {
//                   if (condition_m) {
//                     //Action_6: Assign label of block S
//                     imgLabels_row[c] = imgLabels_row[c - 2];
//                     continue;
//                   }
//                   else {
//                     // ACTION_9 Merge labels of block P and S
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c - 2], imgLabels_row[c - 2]);
//                     continue;
//                   }
//                 }
//                 else {
//                   if (condition_i) {
//                     if (condition_m) {
//                       if (condition_g) {
//                         if (condition_b) {
//                           //Action_6: Assign label of block S
//                           imgLabels_row[c] = imgLabels_row[c - 2];
//                           continue;
//                         }
//                         else {
//                           //Action_11: Merge labels of block Q and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                       else {
//                         //Action_11: Merge labels of block Q and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                     else {
//                       //Action_11: Merge labels of block Q and S
//                       imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                       continue;
//                     }
//                   }
//                   else {
//                     //Action_6: Assign label of block S
//                     imgLabels_row[c] = imgLabels_row[c - 2];
//                     continue;
//                   }
//                 }
//               }
//             }
//           }
//           else {
//             if (condition_j) {
//               if (condition_i) {
//                 //Action_4: Assign label of block Q
//                 imgLabels_row[c] = imgLabels_row_prev_prev[c];
//                 continue;
//               }
//               else {
//                 if (condition_h) {
//                   if (condition_c) {
//                     //Action_4: Assign label of block Q
//                     imgLabels_row[c] = imgLabels_row_prev_prev[c];
//                     continue;
//                   }
//                   else {
//                     //Action_7: Merge labels of block P and Q
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c - 2], imgLabels_row_prev_prev[c]);
//                     continue;
//                   }
//                 }
//                 else {
//                   //Action_4: Assign label of block Q
//                   imgLabels_row[c] = imgLabels_row_prev_prev[c];
//                   continue;
//                 }
//               }
//             }
//             else {
//               if (condition_p) {
//                 if (condition_k) {
//                   if (condition_i) {
//                     if (condition_d) {
//                       //Action_5: Assign label of block R
//                       imgLabels_row[c] = imgLabels_row_prev_prev[c + 2];
//                       continue;
//                     }
//                     else {
//                       // ACTION_10 Merge labels of block Q and R
//                       imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]);
//                       continue;
//                     }
//                   }
//                   else {
//                     if (condition_h) {
//                       if (condition_d) {
//                         if (condition_c) {
//                           //Action_5: Assign label of block R
//                           imgLabels_row[c] = imgLabels_row_prev_prev[c + 2];
//                           continue;
//                         }
//                         else {
//                           //Action_8: Merge labels of block P and R
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c - 2], imgLabels_row_prev_prev[c + 2]);
//                           continue;
//                         }
//                       }
//                       else {
//                         //Action_8: Merge labels of block P and R
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c - 2], imgLabels_row_prev_prev[c + 2]);
//                         continue;
//                       }
//                     }
//                     else {
//                       //Action_5: Assign label of block R
//                       imgLabels_row[c] = imgLabels_row_prev_prev[c + 2];
//                       continue;
//                     }
//                   }
//                 }
//                 else {
//                   if (condition_i) {
//                     //Action_4: Assign label of block Q
//                     imgLabels_row[c] = imgLabels_row_prev_prev[c];
//                     continue;
//                   }
//                   else {
//                     if (condition_h) {
//                       //Action_3: Assign label of block P
//                       imgLabels_row[c] = imgLabels_row_prev_prev[c - 2];
//                       continue;
//                     }
//                     else {
//                       //Action_2: New label (the block has foreground pixels and is not connected to anything else)
//                       imgLabels_row[c] = lunique;
//                       P[lunique] = lunique;
//                       lunique = lunique + 1;
//                       continue;
//                     }
//                   }
//                 }
//               }
//               else {
//                 if (condition_i) {
//                   //Action_4: Assign label of block Q
//                   imgLabels_row[c] = imgLabels_row_prev_prev[c];
//                   continue;
//                 }
//                 else {
//                   if (condition_h) {
//                     //Action_3: Assign label of block P
//                     imgLabels_row[c] = imgLabels_row_prev_prev[c - 2];
//                     continue;
//                   }
//                   else {
//                     //Action_2: New label (the block has foreground pixels and is not connected to anything else)
//                     imgLabels_row[c] = lunique;
//                     P[lunique] = lunique;
//                     lunique = lunique + 1;
//                     continue;
//                   }
//                 }
//               }
//             }
//           }
//         }
//       }
//       else {
//         if (condition_s) {
//           if (condition_p) {
//             if (condition_n) {
//               if (condition_j) {
//                 if (condition_i) {
//                   //Action_6: Assign label of block S
//                   imgLabels_row[c] = imgLabels_row[c - 2];
//                   continue;
//                 }
//                 else {
//                   if (condition_c) {
//                     if (condition_h) {
//                       //Action_6: Assign label of block S
//                       imgLabels_row[c] = imgLabels_row[c - 2];
//                       continue;
//                     }
//                     else {
//                       if (condition_g) {
//                         if (condition_b) {
//                           //Action_6: Assign label of block S
//                           imgLabels_row[c] = imgLabels_row[c - 2];
//                           continue;
//                         }
//                         else {
//                           //Action_11: Merge labels of block Q and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                       else {
//                         //Action_11: Merge labels of block Q and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                   }
//                   else {
//                     //Action_11: Merge labels of block Q and S
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                     continue;
//                   }
//                 }
//               }
//               else {
//                 if (condition_k) {
//                   if (condition_d) {
//                     if (condition_i) {
//                       //Action_6: Assign label of block S
//                       imgLabels_row[c] = imgLabels_row[c - 2];
//                       continue;
//                     }
//                     else {
//                       if (condition_c) {
//                         if (condition_h) {
//                           //Action_6: Assign label of block S
//                           imgLabels_row[c] = imgLabels_row[c - 2];
//                           continue;
//                         }
//                         else {
//                           if (condition_g) {
//                             if (condition_b) {
//                               //Action_6: Assign label of block S
//                               imgLabels_row[c] = imgLabels_row[c - 2];
//                               continue;
//                             }
//                             else {
//                               //Action_12: Merge labels of block R and S
//                               imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                               continue;
//                             }
//                           }
//                           else {
//                             //Action_12: Merge labels of block R and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                       }
//                       else {
//                         //Action_12: Merge labels of block R and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                   }
//                   else {
//                     //Action_12: Merge labels of block R and S
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                     continue;
//                   }
//                 }
//                 else {
//                   //Action_6: Assign label of block S
//                   imgLabels_row[c] = imgLabels_row[c - 2];
//                   continue;
//                 }
//               }
//             }
//             else {
//               if (condition_r) {
//                 if (condition_j) {
//                   if (condition_m) {
//                     if (condition_h) {
//                       if (condition_i) {
//                         //Action_6: Assign label of block S
//                         imgLabels_row[c] = imgLabels_row[c - 2];
//                         continue;
//                       }
//                       else {
//                         if (condition_c) {
//                           //Action_6: Assign label of block S
//                           imgLabels_row[c] = imgLabels_row[c - 2];
//                           continue;
//                         }
//                         else {
//                           //Action_11: Merge labels of block Q and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                     }
//                     else {
//                       if (condition_g) {
//                         if (condition_b) {
//                           if (condition_i) {
//                             //Action_6: Assign label of block S
//                             imgLabels_row[c] = imgLabels_row[c - 2];
//                             continue;
//                           }
//                           else {
//                             if (condition_c) {
//                               //Action_6: Assign label of block S
//                               imgLabels_row[c] = imgLabels_row[c - 2];
//                               continue;
//                             }
//                             else {
//                               //Action_11: Merge labels of block Q and S
//                               imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                               continue;
//                             }
//                           }
//                         }
//                         else {
//                           //Action_11: Merge labels of block Q and S
//                           imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                       else {
//                         //Action_11: Merge labels of block Q and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                   }
//                   else {
//                     //Action_11: Merge labels of block Q and S
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                     continue;
//                   }
//                 }
//                 else {
//                   if (condition_k) {
//                     if (condition_d) {
//                       if (condition_m) {
//                         if (condition_h) {
//                           if (condition_i) {
//                             //Action_6: Assign label of block S
//                             imgLabels_row[c] = imgLabels_row[c - 2];
//                             continue;
//                           }
//                           else {
//                             if (condition_c) {
//                               //Action_6: Assign label of block S
//                               imgLabels_row[c] = imgLabels_row[c - 2];
//                               continue;
//                             }
//                             else {
//                               //Action_12: Merge labels of block R and S
//                               imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                               continue;
//                             }
//                           }
//                         }
//                         else {
//                           if (condition_g) {
//                             if (condition_b) {
//                               if (condition_i) {
//                                 //Action_6: Assign label of block S
//                                 imgLabels_row[c] = imgLabels_row[c - 2];
//                                 continue;
//                               }
//                               else {
//                                 if (condition_c) {
//                                   //Action_6: Assign label of block S
//                                   imgLabels_row[c] = imgLabels_row[c - 2];
//                                   continue;
//                                 }
//                                 else {
//                                   //Action_12: Merge labels of block R and S
//                                   imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                                   continue;
//                                 }
//                               }
//                             }
//                             else {
//                               //Action_12: Merge labels of block R and S
//                               imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                               continue;
//                             }
//                           }
//                           else {
//                             //Action_12: Merge labels of block R and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                       }
//                       else {
//                         //Action_12: Merge labels of block R and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                     else {
//                       if (condition_i) {
//                         if (condition_m) {
//                           if (condition_h) {
//                             //Action_12: Merge labels of block R and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                           else {
//                             if (condition_g) {
//                               if (condition_b) {
//                                 //Action_12: Merge labels of block R and S
//                                 imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                                 continue;
//                               }
//                               else {
//                                 //Action_16: labels of block Q, R and S
//                                 imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]), imgLabels_row[c - 2]);
//                                 continue;
//                               }
//                             }
//                             else {
//                               //Action_16: labels of block Q, R and S
//                               imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]), imgLabels_row[c - 2]);
//                               continue;
//                             }
//                           }
//                         }
//                         else {
//                           //Action_16: labels of block Q, R and S
//                           imgLabels_row[c] = set_union(P, set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]), imgLabels_row[c - 2]);
//                           continue;
//                         }
//                       }
//                       else {
//                         //Action_12: Merge labels of block R and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c + 2], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                   }
//                   else {
//                     if (condition_i) {
//                       if (condition_m) {
//                         if (condition_h) {
//                           //Action_6: Assign label of block S
//                           imgLabels_row[c] = imgLabels_row[c - 2];
//                           continue;
//                         }
//                         else {
//                           if (condition_g) {
//                             if (condition_b) {
//                               //Action_6: Assign label of block S
//                               imgLabels_row[c] = imgLabels_row[c - 2];
//                               continue;
//                             }
//                             else {
//                               //Action_11: Merge labels of block Q and S
//                               imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                               continue;
//                             }
//                           }
//                           else {
//                             //Action_11: Merge labels of block Q and S
//                             imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                             continue;
//                           }
//                         }
//                       }
//                       else {
//                         //Action_11: Merge labels of block Q and S
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row[c - 2]);
//                         continue;
//                       }
//                     }
//                     else {
//                       //Action_6: Assign label of block S
//                       imgLabels_row[c] = imgLabels_row[c - 2];
//                       continue;
//                     }
//                   }
//                 }
//               }
//               else {
//                 if (condition_j) {
//                   //Action_4: Assign label of block Q
//                   imgLabels_row[c] = imgLabels_row_prev_prev[c];
//                   continue;
//                 }
//                 else {
//                   if (condition_k) {
//                     if (condition_i) {
//                       if (condition_d) {
//                         //Action_5: Assign label of block R
//                         imgLabels_row[c] = imgLabels_row_prev_prev[c + 2];
//                         continue;
//                       }
//                       else {
//                         // ACTION_10 Merge labels of block Q and R
//                         imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]);
//                         continue;
//                       }
//                     }
//                     else {
//                       //Action_5: Assign label of block R
//                       imgLabels_row[c] = imgLabels_row_prev_prev[c + 2];
//                       continue;
//                     }
//                   }
//                   else {
//                     if (condition_i) {
//                       //Action_4: Assign label of block Q
//                       imgLabels_row[c] = imgLabels_row_prev_prev[c];
//                       continue;
//                     }
//                     else {
//                       //Action_2: New label (the block has foreground pixels and is not connected to anything else)
//                       imgLabels_row[c] = lunique;
//                       P[lunique] = lunique;
//                       lunique = lunique + 1;
//                       continue;
//                     }
//                   }
//                 }
//               }
//             }
//           }
//           else {
//             if (condition_r) {
//               //Action_6: Assign label of block S
//               imgLabels_row[c] = imgLabels_row[c - 2];
//               continue;
//             }
//             else {
//               if (condition_n) {
//                 //Action_6: Assign label of block S
//                 imgLabels_row[c] = imgLabels_row[c - 2];
//                 continue;
//               }
//               else {
//                 //Action_2: New label (the block has foreground pixels and is not connected to anything else)
//                 imgLabels_row[c] = lunique;
//                 P[lunique] = lunique;
//                 lunique = lunique + 1;
//                 continue;
//               }
//             }
//           }
//         }
//         else {
//           if (condition_p) {
//             if (condition_j) {
//               //Action_4: Assign label of block Q
//               imgLabels_row[c] = imgLabels_row_prev_prev[c];
//               continue;
//             }
//             else {
//               if (condition_k) {
//                 if (condition_i) {
//                   if (condition_d) {
//                     //Action_5: Assign label of block R
//                     imgLabels_row[c] = imgLabels_row_prev_prev[c + 2];
//                     continue;
//                   }
//                   else {
//                     // ACTION_10 Merge labels of block Q and R
//                     imgLabels_row[c] = set_union(P, imgLabels_row_prev_prev[c], imgLabels_row_prev_prev[c + 2]);
//                     continue;
//                   }
//                 }
//                 else {
//                   //Action_5: Assign label of block R
//                   imgLabels_row[c] = imgLabels_row_prev_prev[c + 2];
//                   continue;
//                 }
//               }
//               else {
//                 if (condition_i) {
//                   //Action_4: Assign label of block Q
//                   imgLabels_row[c] = imgLabels_row_prev_prev[c];
//                   continue;
//                 }
//                 else {
//                   //Action_2: New label (the block has foreground pixels and is not connected to anything else)
//                   imgLabels_row[c] = lunique;
//                   P[lunique] = lunique;
//                   lunique = lunique + 1;
//                   continue;
//                 }
//               }
//             }
//           }
//           else {
//             if (condition_t) {
//               //Action_2: New label (the block has foreground pixels and is not connected to anything else)
//               imgLabels_row[c] = lunique;
//               P[lunique] = lunique;
//               lunique = lunique + 1;
//               continue;
//             }
//             else {
//               // Action_1: No action (the block has no foreground pixels)
//               imgLabels_row[c] = 0;
//               continue;
//             }
//           }
//         }
//       }
//     }
//   }
//
// #undef condition_b
// #undef condition_c
// #undef condition_d
// #undef condition_e
//
// #undef condition_g
// #undef condition_h
// #undef condition_i
// #undef condition_j
// #undef condition_k
//
// #undef condition_m
// #undef condition_n
// #undef condition_o
// #undef condition_p
//
// #undef condition_r
// #undef condition_s
// #undef condition_t
//
// }
//
// int BBDT_OPT(const Mat1b &img, Mat1i &imgLabels) {
//
//   imgLabels = cv::Mat1i(img.size());
//   //A quick and dirty upper bound for the maximimum number of labels.
//   const size_t Plength = img.rows*img.cols / 4;
//   //Tree of labels
//   uint *P = (uint *)fastMalloc(sizeof(uint)* Plength);
//   //Background
//   P[0] = 0;
//   uint lunique = 1;
//
//   firstScanBBDT_OPT(img, imgLabels, P, lunique);
//
//   uint nLabel = flattenL(P, lunique);
//
//   // Second scan
//   if (imgLabels.rows & 1){
//     if (imgLabels.cols & 1){
//       //Case 1: both rows and cols odd
//       for (int r = 0; r<imgLabels.rows; r += 2) {
//         // Get rows pointer
//         const uchar* const img_row = img.ptr<uchar>(r);
//         const uchar* const img_row_fol = (uchar *)(((char *)img_row) + img.step.p[0]);
//
//         uint* const imgLabels_row = imgLabels.ptr<uint>(r);
//         uint* const imgLabels_row_fol = (uint *)(((char *)imgLabels_row) + imgLabels.step.p[0]);
//         // Get rows pointer
//         for (int c = 0; c<imgLabels.cols; c += 2) {
//           int iLabel = imgLabels_row[c];
//           if (iLabel>0) {
//             iLabel = P[iLabel];
//             if (img_row[c]>0)
//               imgLabels_row[c] = iLabel;
//             else
//               imgLabels_row[c] = 0;
//             if (c + 1<imgLabels.cols) {
//               if (img_row[c + 1]>0)
//                 imgLabels_row[c + 1] = iLabel;
//               else
//                 imgLabels_row[c + 1] = 0;
//               if (r + 1<imgLabels.rows) {
//                 if (img_row_fol[c]>0)
//                   imgLabels_row_fol[c] = iLabel;
//                 else
//                   imgLabels_row_fol[c] = 0;
//                 if (img_row_fol[c + 1]>0)
//                   imgLabels_row_fol[c + 1] = iLabel;
//                 else
//                   imgLabels_row_fol[c + 1] = 0;
//               }
//             }
//             else if (r + 1<imgLabels.rows) {
//               if (img_row_fol[c]>0)
//                 imgLabels_row_fol[c] = iLabel;
//               else
//                 imgLabels_row_fol[c] = 0;
//             }
//           }
//           else {
//             imgLabels_row[c] = 0;
//             if (c + 1<imgLabels.cols) {
//               imgLabels_row[c + 1] = 0;
//               if (r + 1<imgLabels.rows) {
//                 imgLabels_row_fol[c] = 0;
//                 imgLabels_row_fol[c + 1] = 0;
//               }
//             }
//             else if (r + 1<imgLabels.rows) {
//               imgLabels_row_fol[c] = 0;
//             }
//           }
//         }
//       }
//     }//END Case 1
//     else{
//       //Case 2: only rows odd
//       for (int r = 0; r<imgLabels.rows; r += 2) {
//         // Get rows pointer
//         const uchar* const img_row = img.ptr<uchar>(r);
//         const uchar* const img_row_fol = (uchar *)(((char *)img_row) + img.step.p[0]);
//
//         uint* const imgLabels_row = imgLabels.ptr<uint>(r);
//         uint* const imgLabels_row_fol = (uint *)(((char *)imgLabels_row) + imgLabels.step.p[0]);
//         // Get rows pointer
//         for (int c = 0; c<imgLabels.cols; c += 2) {
//           int iLabel = imgLabels_row[c];
//           if (iLabel>0) {
//             iLabel = P[iLabel];
//             if (img_row[c]>0)
//               imgLabels_row[c] = iLabel;
//             else
//               imgLabels_row[c] = 0;
//             if (img_row[c + 1]>0)
//               imgLabels_row[c + 1] = iLabel;
//             else
//               imgLabels_row[c + 1] = 0;
//             if (r + 1<imgLabels.rows) {
//               if (img_row_fol[c]>0)
//                 imgLabels_row_fol[c] = iLabel;
//               else
//                 imgLabels_row_fol[c] = 0;
//               if (img_row_fol[c + 1]>0)
//                 imgLabels_row_fol[c + 1] = iLabel;
//               else
//                 imgLabels_row_fol[c + 1] = 0;
//             }
//           }
//           else {
//             imgLabels_row[c] = 0;
//             imgLabels_row[c + 1] = 0;
//             if (r + 1<imgLabels.rows) {
//               imgLabels_row_fol[c] = 0;
//               imgLabels_row_fol[c + 1] = 0;
//             }
//           }
//         }
//       }
//     }// END Case 2
//   }
//   else{
//     if (imgLabels.cols & 1){
//       //Case 3: only cols odd
//       for (int r = 0; r<imgLabels.rows; r += 2) {
//         // Get rows pointer
//         const uchar* const img_row = img.ptr<uchar>(r);
//         const uchar* const img_row_fol = (uchar *)(((char *)img_row) + img.step.p[0]);
//
//         uint* const imgLabels_row = imgLabels.ptr<uint>(r);
//         uint* const imgLabels_row_fol = (uint *)(((char *)imgLabels_row) + imgLabels.step.p[0]);
//         // Get rows pointer
//         for (int c = 0; c<imgLabels.cols; c += 2) {
//           int iLabel = imgLabels_row[c];
//           if (iLabel>0) {
//             iLabel = P[iLabel];
//             if (img_row[c]>0)
//               imgLabels_row[c] = iLabel;
//             else
//               imgLabels_row[c] = 0;
//             if (img_row_fol[c]>0)
//               imgLabels_row_fol[c] = iLabel;
//             else
//               imgLabels_row_fol[c] = 0;
//             if (c + 1<imgLabels.cols) {
//               if (img_row[c + 1]>0)
//                 imgLabels_row[c + 1] = iLabel;
//               else
//                 imgLabels_row[c + 1] = 0;
//               if (img_row_fol[c + 1]>0)
//                 imgLabels_row_fol[c + 1] = iLabel;
//               else
//                 imgLabels_row_fol[c + 1] = 0;
//             }
//           }
//           else{
//             imgLabels_row[c] = 0;
//             imgLabels_row_fol[c] = 0;
//             if (c + 1<imgLabels.cols) {
//               imgLabels_row[c + 1] = 0;
//               imgLabels_row_fol[c + 1] = 0;
//             }
//           }
//         }
//       }
//     }// END case 3
//     else{
//       //Case 4: nothing odd
//       for (int r = 0; r < imgLabels.rows; r += 2) {
//         // Get rows pointer
//         const uchar* const img_row = img.ptr<uchar>(r);
//         const uchar* const img_row_fol = (uchar *)(((char *)img_row) + img.step.p[0]);
//
//         uint* const imgLabels_row = imgLabels.ptr<uint>(r);
//         uint* const imgLabels_row_fol = (uint *)(((char *)imgLabels_row) + imgLabels.step.p[0]);
//         // Get rows pointer
//         for (int c = 0; c<imgLabels.cols; c += 2) {
//           int iLabel = imgLabels_row[c];
//           if (iLabel>0) {
//             iLabel = P[iLabel];
//             if (img_row[c] > 0)
//               imgLabels_row[c] = iLabel;
//             else
//               imgLabels_row[c] = 0;
//             if (img_row[c + 1] > 0)
//               imgLabels_row[c + 1] = iLabel;
//             else
//               imgLabels_row[c + 1] = 0;
//             if (img_row_fol[c] > 0)
//               imgLabels_row_fol[c] = iLabel;
//             else
//               imgLabels_row_fol[c] = 0;
//             if (img_row_fol[c + 1] > 0)
//               imgLabels_row_fol[c + 1] = iLabel;
//             else
//               imgLabels_row_fol[c + 1] = 0;
//           }
//           else {
//             imgLabels_row[c] = 0;
//             imgLabels_row[c + 1] = 0;
//             imgLabels_row_fol[c] = 0;
//             imgLabels_row_fol[c + 1] = 0;
//           }
//         }
//       }
//     }//END case 4
//   }
//
//   fastFree(P);
//   return nLabel;
// }
//
// // adapterd after https://github.com/prittt/YACCLAB/blob/master/src/labelingGrana2016.cpp
// void firstScan(const Mat1b &img, Mat1i& imgLabels, uint* P, uint &lunique) {
//   int width(img.cols), h(img.rows);
//
// #define condition_x img(r,c)>0
// #define condition_p img(r-1,c-1)>0
// #define condition_q img(r-1,c)>0
// #define condition_r img(r-1,c+1)>0
//
// {
//   int r = 0;
//   int c = -1;
//   tree_A0: if (++c >= width) goto break_A0;
//   if (condition_x) {
//     // x = new label
//     imgLabels(r, c) = lunique;
//     P[lunique] = lunique;
//     lunique++;
//     goto tree_B0;
//   }
//   else {
//     // nothing
//     goto tree_A0;
//   }
//   tree_B0: if (++c >= width) goto break_B0;
//   if (condition_x) {
//     imgLabels(r, c) = imgLabels(r , c - 1); // x = s
//     goto tree_B0;
//   }
//   else {
//     // nothing
//     goto tree_A0;
//   }
//   break_A0:
//     break_B0 : ;
// }
//
// for (int r = 1; r < height; ++r) {
//   // First column
//   int c = 0;
//   if (condition_x) {
//     if (condition_q) {
//       imgLabels(r, c) = imgLabels(r - 1, c); // x = q
//       goto tree_A;
//     }
//     else {
//       if (condition_r) {
//         imgLabels(r,c) = imgLabels(r - 1, c + 1); // x = r
//         goto tree_B;
//       }
//       else {
//         // x = new label
//         imgLabels(r,c) = lunique;
//         P[lunique] = lunique;
//         lunique++;
//         goto tree_C;
//       }
//     }
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//
//   tree_A: if (++c >= width - 1) goto break_A;
//   if (condition_x) {
//     if (condition_q) {
//       imgLabels(r, c) = imgLabels(r - 1, c); // x = q
//       goto tree_A;
//     }
//     else {
//       if (condition_r) {
//         imgLabels(r, c) = set_union(P, (uint)imgLabels(r - 1, c + 1), (uint)imgLabels(r, c - 1)); // x = r + s
//         goto tree_B;
//       }
//       else {
//         imgLabels(r,c) = imgLabels(r,c - 1); // x = s
//         goto tree_C;
//       }
//     }
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//   tree_B: if (++c >= width - 1) goto break_B;
//   if (condition_x) {
//     imgLabels(r,c) = imgLabels(r - 1, c); // x = q
//     goto tree_A;
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//   tree_C: if (++c >= width - 1) goto break_C;
//   if (condition_x) {
//     if (condition_r) {
//       imgLabels(r, c) = set_union(P, (uint)imgLabels(r - 1, c + 1), (uint)imgLabels(r, c - 1)); // x = r + s
//       goto tree_B;
//     }
//     else {
//       imgLabels(r,c) = imgLabels(r,c - 1); // x = s
//       goto tree_C;
//     }
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//   tree_D: if (++c >= width - 1) goto break_D;
//   if (condition_x) {
//     if (condition_q) {
//       imgLabels(r,c) = imgLabels(r-1, c); // x = q
//       goto tree_A;
//     }
//     else {
//       if (condition_r) {
//         if (condition_p) {
//           imgLabels(r, c) = set_union(P, (uint)imgLabels(r - 1, c - 1), (uint)imgLabels(r - 1, c + 1)); // x = p + r
//           goto tree_B;
//         }
//         else {
//           imgLabels(r,c) = imgLabels(r - 1, c + 1); // x = r
//           goto tree_B;
//         }
//       }
//       else {
//         if (condition_p) {
//           imgLabels(r, c) = imgLabels(r - 1, c - 1); // x = p
//           goto tree_C;
//         }
//         else {
//           // x = new label
//           imgLabels(r,c) = lunique;
//           P[lunique] = lunique;
//           lunique++;
//           goto tree_C;
//         }
//       }
//     }
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//
//
//   // Last column
//   break_A:
//     if (condition_x) {
//       if (condition_q) {
//         imgLabels(r, c) = imgLabels(r - 1, c); // x = q
//       }
//       else {
//         imgLabels(r,c) = imgLabels(r,c - 1); // x = s
//       }
//     }
//     continue;
//     break_B:
//       if (condition_x) {
//         imgLabels(r,c) = imgLabels(r - 1, c); // x = q
//       }
//       continue;
//       break_C:
//         if (condition_x) {
//           imgLabels(r,c) = imgLabels(r,c - 1); // x = s
//         }
//         continue;
//         break_D:
//           if (condition_x) {
//             if (condition_q) {
//               imgLabels(r,c) = imgLabels(r - 1, c); // x = q
//             }
//             else {
//               if (condition_p) {
//                 imgLabels(r, c) = imgLabels(r - 1, c - 1); // x = p
//               }
//               else {
//                 // x = new label
//                 imgLabels(r,c) = lunique;
//                 P[lunique] = lunique;
//                 lunique++;
//               }
//             }
//           }
// }//End rows's for
//
// #undef condition_x
// #undef condition_p
// #undef condition_q
// #undef condition_r
// }
//
// int PRED(const Mat1b &img, Mat1i &imgLabels) {
//
//   imgLabels = cv::Mat1i(img.size(), 0); // memset is used
//   //A quick and dirty upper bound for the maximimum number of labels.
//   const size_t Plength = img.rows*img.cols / 4;
//   //Tree of labels
//   vector<uint> P(Plength);
//   //Background
//   P[0] = 0;
//   uint lunique = 1;
//
//   firstScan(img, imgLabels, P.data(), lunique);
//
//   uint nLabel = flattenL(P.data(), lunique);
//
//   // second scan
//   for (int r_i = 0; r_i < imgLabels.rows; ++r_i) {
//     for (int c_i = 0; c_i < imgLabels.cols; ++c_i){
//       imgLabels(r_i, c_i) = P[imgLabels(r_i, c_i)];
//     }
//   }
//
//   return nLabel;
// }
//
// void firstScan_OPT(const Mat1b &img, Mat1i& imgLabels, uint* P, uint &lunique) {
//   int width(img.cols), height(img.rows);
//
// #define condition_x img_row[c]>0
// #define condition_p img_row_prev[c-1]>0
// #define condition_q img_row_prev[c]>0
// #define condition_r img_row_prev[c+1]>0
//
// {
//   // Get rows pointer
//   const uchar* const img_row = img.ptr<uchar>(0);
//   uint* const imgLabels_row = imgLabels.ptr<uint>(0);
//
//   int c = -1;
//   tree_A0: if (++c >= width) goto break_A0;
//   if (condition_x) {
//     // x = new label
//     imgLabels_row[c] = lunique;
//     P[lunique] = lunique;
//     lunique++;
//     goto tree_B0;
//   }
//   else {
//     // nothing
//     goto tree_A0;
//   }
//   tree_B0: if (++c >= width) goto break_B0;
//   if (condition_x) {
//     imgLabels_row[c] = imgLabels_row[c - 1]; // x = s
//     goto tree_B0;
//   }
//   else {
//     // nothing
//     goto tree_A0;
//   }
//   break_A0:
//     break_B0: ;
// }
//
// for (int r = 1; r < height; ++r) {
//   // Get rows pointer
//   const uchar* const img_row = img.ptr<uchar>(r);
//   const uchar* const img_row_prev = (uchar *)(((char *)img_row) - img.step.p[0]);
//   uint* const imgLabels_row = imgLabels.ptr<uint>(r);
//   uint* const imgLabels_row_prev = (uint *)(((char *)imgLabels_row) - imgLabels.step.p[0]);
//
//   // First column
//   int c = 0;
//   if (condition_x) {
//     if (condition_q) {
//       imgLabels_row[c] = imgLabels_row_prev[c]; // x = q
//       goto tree_A;
//     }
//     else {
//       if (condition_r) {
//         imgLabels_row[c] = imgLabels_row_prev[c + 1]; // x = r
//         goto tree_B;
//       }
//       else {
//         // x = new label
//         imgLabels_row[c] = lunique;
//         P[lunique] = lunique;
//         lunique++;
//         goto tree_C;
//       }
//     }
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//
//   tree_A: if (++c >= width - 1) goto break_A;
//   if (condition_x) {
//     if (condition_q) {
//       imgLabels_row[c] = imgLabels_row_prev[c]; // x = q
//       goto tree_A;
//     }
//     else {
//       if (condition_r) {
//         imgLabels_row[c] = set_union(P, imgLabels_row_prev[c + 1], imgLabels_row[c - 1]); // x = r + s
//         goto tree_B;
//       }
//       else {
//         imgLabels_row[c] = imgLabels_row[c - 1]; // x = s
//         goto tree_C;
//       }
//     }
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//   tree_B: if (++c >= width - 1) goto break_B;
//   if (condition_x) {
//     imgLabels_row[c] = imgLabels_row_prev[c]; // x = q
//     goto tree_A;
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//   tree_C: if (++c >= width - 1) goto break_C;
//   if (condition_x) {
//     if (condition_r) {
//       imgLabels_row[c] = set_union(P, imgLabels_row_prev[c + 1], imgLabels_row[c - 1]); // x = r + s
//       goto tree_B;
//     }
//     else {
//       imgLabels_row[c] = imgLabels_row[c - 1]; // x = s
//       goto tree_C;
//     }
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//   tree_D: if (++c >= width - 1) goto break_D;
//   if (condition_x) {
//     if (condition_q) {
//       imgLabels_row[c] = imgLabels_row_prev[c]; // x = q
//       goto tree_A;
//     }
//     else {
//       if (condition_r) {
//         if (condition_p) {
//           imgLabels_row[c] = set_union(P, imgLabels_row_prev[c - 1], imgLabels_row_prev[c + 1]); // x = p + r
//           goto tree_B;
//         }
//         else {
//           imgLabels_row[c] = imgLabels_row_prev[c + 1]; // x = r
//           goto tree_B;
//         }
//       }
//       else {
//         if (condition_p) {
//           imgLabels_row[c] = imgLabels_row_prev[c - 1]; // x = p
//           goto tree_C;
//         }
//         else {
//           // x = new label
//           imgLabels_row[c] = lunique;
//           P[lunique] = lunique;
//           lunique++;
//           goto tree_C;
//         }
//       }
//     }
//   }
//   else {
//     // nothing
//     goto tree_D;
//   }
//
//
//   // Last column
//   break_A:
//     if (condition_x) {
//       if (condition_q) {
//         imgLabels_row[c] = imgLabels_row_prev[c]; // x = q
//       }
//       else {
//         imgLabels_row[c] = imgLabels_row[c - 1]; // x = s
//       }
//     }
//     continue;
//     break_B:
//       if (condition_x) {
//         imgLabels_row[c] = imgLabels_row_prev[c]; // x = q
//       }
//       continue;
//       break_C:
//         if (condition_x) {
//           imgLabels_row[c] = imgLabels_row[c - 1]; // x = s
//         }
//         continue;
//         break_D:
//           if (condition_x) {
//             if (condition_q) {
//               imgLabels_row[c] = imgLabels_row_prev[c]; // x = q
//             }
//             else {
//               if (condition_p) {
//                 imgLabels_row[c] = imgLabels_row_prev[c - 1]; // x = p
//               }
//               else {
//                 // x = new label
//                 imgLabels_row[c] = lunique;
//                 P[lunique] = lunique;
//                 lunique++;
//               }
//             }
//           }
// }//End rows's for
//
// #undef condition_x
// #undef condition_p
// #undef condition_q
// #undef condition_r
// }
//
// int PRED_OPT(const Mat1b &img, Mat1i &imgLabels) {
//
//   imgLabels = cv::Mat1i(img.size(),0); // memset is used
//   //A quick and dirty upper bound for the maximimum number of labels.
//   const size_t Plength = img.rows*img.cols / 4;
//   //Tree of labels
//   uint *P = (uint *)fastMalloc(sizeof(uint)* Plength);
//   //Background
//   P[0] = 0;
//   uint lunique = 1;
//
//   firstScan_OPT(img, imgLabels, P, lunique);
//
//   uint nLabel = flattenL(P, lunique);
//
//   // second scan
//   for (int r_i = 0; r_i < imgLabels.rows; ++r_i) {
//     uint *b = imgLabels.ptr<uint>(r_i);
//     uint *e = b + imgLabels.cols;
//     for (; b != e; ++b){
//       *b = P[*b];
//     }
//   }
//
//   fastFree(P);
//   return nLabel;
// }
