{-

Đề bài yêu cầu viết một hàm có tên `quảnLýĐiểmSốSinhViên` nhận danh sách các sinh viên và điểm số của họ dưới dạng một danh sách tuples. Hàm này sẽ tính toán điểm trung bình, phân loại điểm và xác định liệu sinh viên đã đạt hay không dựa trên điểm trung bình của họ.
**Cụ thể:**
- Hàm sẽ nhận đầu vào là một danh sách các tuples, mỗi tuple gồm tên của sinh viên và danh sách điểm của họ.
- Hàm sẽ trả về một danh sách các tuples mới, trong đó mỗi tuple bao gồm tên của sinh viên, danh sách điểm, điểm trung bình, phân loại điểm (xuất sắc, giỏi, khá, trung bình, yếu), và kết quả xác định liệu sinh viên đã đạt hay không (đạt hoặc không đạt).
Các Bước Làm:
1. **Phân loại điểm**:
   - Sử dụng các guard để xác định loại điểm của sinh viên, từ "Xuất sắc" đến "Yếu".
   - Dùng một hàm riêng để thực hiện phân loại điểm.

2. **Tính điểm trung bình**:
   - Tính tổng của các điểm trong danh sách và chia cho số lượng điểm để tính điểm trung bình.
   - Sử dụng hàm `fromIntegral` để chuyển đổi kiểu dữ liệu.

3. **Xác định kết quả đạt**:
   - Sử dụng `case` expression để kiểm tra xem điểm trung bình có đạt hay không
   - Trả về "Đạt" nếu điểm trung bình lớn hơn hoặc bằng 45, ngược lại trả về "Không đạt".

4. **Xử lý sinh viên**:
   - Áp dụng các bước phân loại điểm, tính điểm trung bình và xác định kết quả đạt cho mỗi sinh viên.
   - Trả về một bộ dữ liệu chứa tên, điểm, điểm trung bình, phân loại điểm và kết quả đạt.

5. **Quản lý điểm số sinh viên**:
   - Duyệt qua từng sinh viên trong danh sách và áp dụng quy trình xử lý sinh viên cho mỗi sinh viên.
   - Gom các kết quả lại thành một danh sách và trả về danh sách đó.

-}


-- Hàm quản lý điểm số sinh viên

quanLyDiem :: [(String,[Float])] -> [(String,[Float],Float,String,String)]
quanLyDiem []=[]
quanLyDiem((ten,diem):dsconlai) = xuLySinhVien(ten,diem) : quanLyDiem dsconlai
   where 
      phanloaiSV :: Float -> String 
      phanloaiSV diem
         | diem >=9 && diem<=10 = "Xuat sac"
         | diem >=8 = "Gioi"
         | diem >=7 = "Kha"
         | diem >=6 = "Trung binh"
         | otherwise = "Yeu"

      diemTrungBinh :: [Float] -> Float
      diemTrungBinh diem = (sum diem)/ fromIntegral (length diem)

      danhGiaSV :: Float -> String
      danhGiaSV diem = case diem >= 6 of
         True -> "Dat"
         False -> "Khong dat"

      xuLySinhVien :: (String,[Float]) -> (String,[Float],Float,String,String)
      xuLySinhVien (ten,diem) = let 
                                    trungBinh = diemTrungBinh diem
                                    danhgia = danhGiaSV trungBinh
                                 in (ten,diem,trungBinh,phanloaiSV trungBinh,danhgia)



