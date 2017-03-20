	;;;2016-12-17开始断断续续重写alisp自定义函数库 made by ice 924604042@qq.com
;;;编写原则：	1，名字望文知义
;;;		2，代码通俗易懂
;;;		3，尽量精简
;;;		4，无需刻意写，即用即写，每用一次优化一次
;;;		5，减少无用代码
;;;		6，尽量保持函数低耦合，高内聚

;;;搁置的瓶颈程序列表
;;;1，打断于点，重绘。
;;;2，最短路径
;;;3，多段线点更新

;;;最后更新：2017/03.13

;;;************************************************************
;;;序  号：
;;;名  字：
;;;功  能：
;;;参  数：
;;;返回值：
;;;关键字：
;;;示  例：
;;;时  间：
;;;************************************************************

;;;************************************************************
;;;序  号：0082
;;;名  字：ice-dimordinate-unique
;;;功  能：
;;;参  数：
;;;返回值：
;;;关键字：
;;;示  例：
;;;时  间：
;;;************************************************************

(defun	ice-dimordinate-unique(lst)
  (if lst
    (cons (car lst) (ice-dimordinate-unique (vl-remove-if ''((x) (equal (cadr x)
					       (cadr (car lst)) 1e-4)) (cdr lst))))))

;;;************************************************************
;;;序  号：0081
;;;名  字：ice-point-avoid
;;;功  能：点避让
;;;参  数：
;;;返回值：
;;;关键字：
;;;示  例：
;;;时  间：
;;;************************************************************

(defun	ice-point-avoid(lst an d / lst0)
  (setq lst0 (list (car lst))
	lst (cdr lst))
  (while lst
    (if (or (= (+ pi an) (angle (car lst0) (car lst)))
	    (> d (distance (car lst0) (car lst))))
	(setq lst0 (cons (polar (car lst0) an d) lst0))
      (setq lst0 (cons (car lst) lst0)))
    (setq lst (cdr lst)))
  (reverse lst0))

;;;************************************************************
;;;序  号：0080
;;;名  字：ice-direction-extend
;;;功  能：根据坐标符号设定方向延伸
;;;参  数：lst -> (x y z)
;;;返回值：(x y z)
;;;关键字：x为正+d，为负-d
;;;示  例：
;;;时  间：2017.03.17
;;;************************************************************

(defun	ice-direction-extend(lst d)
  (mapcar ''((x) (if (minusp x) (- x d) (+ x d))) lst))

;;;************************************************************
;;;序  号：0079
;;;名  字：ice-2p->pxy
;;;功  能：源点和终点推导出此方向xy距离d的坐标
;;;参  数：lst -> ((x0 y0 z0) (x1 y1 z1))，d -> distance
;;;返回值：(x方向的点，y方向的点)
;;;关键字：相对点
;;;示  例：
;;;时  间：2017.03.17
;;;************************************************************

(defun	ice-2p->pxy(lst d)
  (mapcar ''((x y) (polar (car lst) x y)) (list 0 (* 0.5 pi))
	  (ice-direction-extend (apply 'mapcar (cons '- (reverse lst))) d)))

;;;************************************************************
;;;序  号：0078
;;;名  字：ice-GetBoundingBox*
;;;功  能：图元列表的最小包围盒
;;;参  数：(obj obj obj ...)
;;;返回值：((min min) (max max))
;;;关键字：
;;;示  例：
;;;时  间：2017.03.15
;;;************************************************************

(defun	ice-GetBoundingBox*(lst)
  (setq lst (apply 'append (mapcar 'ice-GetBoundingBox lst)))
  (mapcar ''((x) (apply 'mapcar (cons x lst))) '(min max)))

;;;************************************************************
;;;序  号：0077
;;;名  字：ice-close-selected
;;;功  能：
;;;参  数：
;;;返回值：
;;;思  路：
;;;备  注：
;;;示  例：
;;;时  间：
;;;************************************************************

(defun	ice-close-selected(obj n flag / lst)
  (setq lst (ice-curve-div obj n))
  (if flag (setq flag "_cp") (setq flag "_wp"))
  (ice-ss->list (ssget flag lst)))

;;;************************************************************
;;;序  号：0076
;;;名  字：ice-list-split
;;;功  能：
;;;参  数：
;;;返回值：
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun ice-list-split(lst n)
  (if (< n (length lst))
    (cons (ice-list-child lst 0 n) (ice-list-split (ice-list-child lst n (length lst)) n))
    (list lst)))

;;;************************************************************
;;;序  号：0075
;;;名  字：ice-list-child
;;;功  能：列表子列表
;;;参  数：
;;;返回值：
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-list-child(lst start end)
  (and (> end (length lst)) (setq end (length lst)))
  (if (< start end)
    (cons (nth start lst) (ice-list-child lst (1+ start) end))))

;;;************************************************************
;;;序  号：0074
;;;名  字：ice-average2
;;;功  能：两数平均值
;;;参  数：
;;;返回值：
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-average2(a b)
  (* 0.5 (+ a b)))

;;;************************************************************
;;;序  号：0073
;;;名  字：ice->3p
;;;功  能：点列表（(x,y) or (x,y,z)）-> (x,y,z)
;;;参  数：p -> list
;;;返回值：p -> list(x,y,z)
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice->3p(p)
  (cond
    ((= 3 (length p)) p)
    ((= 2 (length p)) (append p '(0.0)))))

;;;************************************************************
;;;序  号：0072
;;;名  字：ice->2p
;;;功  能：点列表（(x,y) or (x,y,z)）-> (x,y)
;;;参  数：p -> list
;;;返回值：p -> list(x,y)
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice->2p(p)
  (cond
    ((= 2 (length p)) p)
    ((= 3 (length p)) (reverse (cdr (reverse p))))))

;;;************************************************************
;;;序  号：0071
;;;名  字：ice-point-polar-xy
;;;功  能：一点相对于另一点xy两边出界指定距离
;;;参  数：lst -> 相对点，源点，d -> 超出界限距离
;;;返回值：x，y的坐标
;;;思  路：
;;;备  注：标注类临时使用
;;;示  例：
;;;************************************************************

(defun	ice-point-polar-xy(lst d / x y)
  (setq x (if (minusp (apply '- (mapcar 'car lst)))
	    pi
	    0)
	y (if (minusp (apply '- (mapcar 'cadr lst)))
	    (* 1.5 pi)
	    (* 0.5 pi)))
  (list
    (polar (cadr lst) x (+ d (abs (apply '- (mapcar 'car lst)))))
    (polar (cadr lst) y (+ d (abs (apply '- (mapcar 'cadr lst)))))))

;;;************************************************************
;;;序  号：0070
;;;名  字：ice-2p->8p
;;;功  能：四分象限定位点
;;;参  数：lst -> 任意能定位矩形的两点（乱序）
;;;返回值：源矩形田字形等分之后四个矩形左下角右上角定位点列表
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-2p->8p(lst)
  (setq lst (apply 'mapcar (cons 'list lst)))
  (mapcar ''((x) (mapcar 'apply x lst)) '((min min) (ice-average2 ice-average2)
					  (ice-average2 min) (max ice-average2)
					  (ice-average2 ice-average2) (max max)
					  (min ice-average2) (ice-average2 max))))

;;;************************************************************
;;;序  号：0069
;;;名  字：ice-2p->4p
;;;功  能：定位矩形的任意两点-> 四点
;;;参  数：定位矩形的任意两点
;;;返回值：（左下角，右下角，右上角，左上角）
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-2p->4p(lst)
  (setq lst (apply 'mapcar (cons 'list lst)))
  (mapcar ''((x) (mapcar 'apply x lst)) '((min min) (max min) (max max) (min max))))

;;;变态写法
;;;(defun	ice-2p->4p(lst)
;;;  (mapcar ''((x) (mapcar 'eval (apply 'mapcar (cons 'list (cons x lst)))))
;;;	  '((min min) (max min)
;;;	    (max max) (min max))))

;;;************************************************************
;;;序  号：0068
;;;名  字：ice-3p->2p
;;;功  能：xyz->xy
;;;参  数：(x,y,z)
;;;返回值：(x,y)
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-3p->2p(p)
  (reverse (cdr (reverse p))))
	
;;;************************************************************
;;;序  号：0067
;;;名  字：ice-GetBoundingbox
;;;功  能：取得图元外包围框，左下角，右上角
;;;参  数：图元
;;;返回值：(左下角，右上角)
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-GetBoundingbox(obj / a b)  
  (vl-load-com)
  (vla-GetBoundingBox (ice->obj obj) 'a 'b)
  (mapcar 'vlax-safearray->list (list a b)))

;;;************************************************************
;;;序  号：0066
;;;名  字：ice-point->rectangle
;;;功  能：点是否在矩形内？
;;;参  数：boolean
;;;返回值：
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-point->rectangle(p lst)
  (vl-every '>= (mapcar '* (mapcar '- p (car lst)) (mapcar '- (cadr lst) p)) '(0 0)))

;;;************************************************************
;;;序  号：0065
;;;名  字：ice-AddDimordinate
;;;功  能：坐标标注
;;;参  数：p0 -> 起点，p1 -> 终点，flag -> 0 x 1 y
;;;返回值：
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-AddDimordinate(p0 p1 flag / myms)
  (vl-load-com)
  (setq myms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (vla-AddDimordinate myms (vlax-3d-point p0) (vlax-3d-point p1) flag))

;;;************************************************************
;;;序  号：0064
;;;名  字：ice->polyline
;;;功  能：合并多段线
;;;参  数：
;;;返回值：
;;;思  路：
;;;备  注：2017.02.21
;;;示  例：
;;;************************************************************

(defun	ice->polyline(s)
  (vl-cmdf "pedit" "m" s "" "" "j" "" ""))

;;;************************************************************
;;;序  号：0063
;;;名  字：ice-overlook-Angle-dimension
;;;功  能：松羽俯视图角度标记
;;;参  数：e -> 圆图元，lst -> 角度列表，sc0 -> 线长比例，sc1 -> 文字基点位置比例，sc2 -> 字高比例 
;;;返回值：无
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

;;;名字：ice-overlook-Angle-dimension
;;;功能：松羽俯视图角度标记
;;;sc0 - 直线:半径		1.1
;;;sc1 - 多行文字点:半径	1.2
;;;sc2 - 多行文字高:半径	0.08

(defun	ice-overlook-angle-dimension(lst / ice-angle-sy
				     e lst sc0 sc1 sc2
				     p r)
;;;名字：ice-Angle-sy
;;;功能：松羽角度->正常角度值（整数）
(defun	ice-angle-sy(a)
  (read (angtos (angtof (vl-princ-to-string (- 180 a))))))

  (mapcar 'set '(e lst sc0 sc1 sc2) lst)
  
  (setq p (ice-get-Dxfvalue e 10)
	r (ice-get-Dxfvalue e 40))
  (mapcar ''((x y) (ice-put-color (ice-AddLine p (polar p x (* sc0 r))) 1)
	     (ice-put-color (ice-AddMtext (polar p x (* sc1 r))
	       (strcat (vl-princ-to-string y) "%%d")
	       (* sc2 r)) 2)
	     (ice-put-dxfvalue (entlast) 71 5))
	  (mapcar ''((x) (angtof (vl-princ-to-string (ice-angle-sy x)))) lst) lst))

;;;************************************************************
;;;序  号：0062
;;;名  字：ice-main
;;;功  能：生成主工具箱模板
;;;参  数：title -> 对话框标题，lst -> ((注释 . 命令)....)，column -> 列数
;;;返回值：无
;;;思  路：
;;;备  注：
;;;示  例：
;;;依  赖：ice-range
;;;************************************************************

(defun	ice-main(title lst column / key label cmd row n filename f dcl_id dd)
  (vl-load-com)
  (setq key "bt"
	label (mapcar 'car lst)
	cmd (mapcar 'cdr lst)
	row (if (zerop (rem (length lst) column))
	      (/ (length lst) column)
	      (1+ (/ (length lst) column)))
	n 0
	filename (strcat (vl-filename-mktemp) ".dcl")
	f (open filename "w"))
  (write-line "xx:dialog{" f)
  (write-line (strcat "label = \"" title "\";") f)
  (write-line ":boxed_column{" f)
  (repeat row
    (write-line ":row{" f)
    (repeat column
      (write-line (strcat ":button{"
			  "label = \"" (if (nth n label) (nth n label) "O(∩_∩)O")
			  "\";"
			  "key = \"" key (itoa (+ 2 n)) "\";"
			  "vertical_margin = none; horizontal_margin = none;}") f)
      (setq n (1+ n)))
    (write-line "}" f))
  (write-line "}" f)
  (write-line "cancel_button;}" f)
  (close f)
  
  (setq dcl_id (load_dialog filename))
  (new_dialog "xx" dcl_id)
  (mapcar ''((x) (action_tile
		  (strcat key (itoa (1+ x)))
		  (strcat "(done_dialog " (itoa (1+ x)) ")"))) (ice-range 1 (1+ (length label)) 1))
  (setq dd (start_dialog))
  (if (and (>= dd 2) (<= dd (1+ (length label))))
    (vla-SendCommand (vla-Get-ActiveDocument (vlax-Get-Acad-Object)) (strcat (nth (- dd 2) cmd) "\n")))
  (unload_dialog dcl_id)
  (vl-file-delete filename))

;;;************************************************************
;;;序  号：0061
;;;名  字：ice-ehb
;;;功  能：创建B类椭圆封头
;;;参  数：p -> 插入点（椭圆中点），d -> 公称直径，thick -> 厚度
;;;返回值：无
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-ehb(p d thick / h p0 p1 p2 p3)
  (setq h (if (> d 2000) 40 25))
  (ice-AddEllipse p (* 0.5 d) (* 0.25 d) pi (* 2 pi))
  (ice-AddEllipse p (- (* 0.5 d) thick) (- (* 0.25 d) thick) pi (* 2 pi))
  (setq p0 (ice-polar-xy p (list (- (* 0.5 d)) 0))
	p1 (ice-polar-xy p (list (- (- (* 0.5 d) thick)) 0))
	p2 (ice-polar-xy p (list (- (* 0.5 d) thick) 0))
	p3 (ice-polar-xy p (list (* 0.5 d) 0)))
  (mapcar ''((x) (ice-AddLine x (ice-polar-xy x (list 0 h)))) (list p0 p1 p2 p3))
  (ice-AddLine p0 p3)
  (ice-AddLine (ice-polar-xy p0 (list 0 h)) (ice-polar-xy p3 (list 0 h))))

;;;************************************************************
;;;序  号：0060
;;;名  字：ice-eha
;;;功  能：创建A类椭圆封头
;;;参  数：p -> 插入点（椭圆中点），d -> 公称直径，thick -> 厚度
;;;返回值：无
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-eha(p d thick / h p0 p1 p2 p3)
  (setq h (if (> d 2000) 40 25))
  (ice-AddEllipse p (* 0.5 d) (* 0.25 d) pi (* 2 pi))
  (ice-AddEllipse p (+ thick (* 0.5 d)) (+ thick (* 0.25 d)) pi (* 2 pi))
  (setq p0 (ice-polar-xy p (list (- (+ thick (* 0.5 d))) 0))
	p1 (ice-polar-xy p (list (- (* 0.5 d)) 0))
	p2 (ice-polar-xy p (list (* 0.5 d) 0))
	p3 (ice-polar-xy p (list (+ thick (* 0.5 d)) 0)))
  (mapcar ''((x) (ice-AddLine x (ice-polar-xy x (list 0 h)))) (list p0 p1 p2 p3))
  (ice-AddLine p0 p3)
  (ice-AddLine (ice-polar-xy p0 (list 0 h)) (ice-polar-xy p3 (list 0 h))))

;;;************************************************************
;;;序  号：0059
;;;名  字：ice-AddEllipse
;;;功  能：创建椭圆或者椭圆弧
;;;参  数：p -> 椭圆中心点，major -> 横向半轴，minor -> 纵向半轴
;;;返回值：
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-AddEllipse(p major minor start end / myms obj)
  (vl-load-com)
  (setq myms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (vla-addellipse myms (vlax-3d-point p)
    (vlax-3d-point major 0 0) (/ (* 1.0 minor) major))
  (setq obj (ice->obj (entlast)))
  (vla-put-startangle obj start)
  (vla-put-endangle obj end)
  (ice->ename obj))

;;;************************************************************
;;;序  号：0058
;;;名  字：ice-hha
;;;功  能：创建标准半球形封头
;;;参  数：p -> 插入点，d -> 公称直径，thick -> 厚度
;;;返回值：selection，整个半球形封头的选择集
;;;思  路：
;;;备  注：
;;;示  例：
;;;日  期：2017.02.18
;;;编  写：ice
;;;************************************************************

(defun	ice-hha(p d thick / s)
  (setq s (ssadd))
  (ice-AddArc p (* 0.5 d) pi 0)
  (ssadd (entlast) s)
  (ice-offset (entlast) thick)
  (ssadd (entlast) s)
  (ice-AddLine
    (ice-polar-xy p (list (- (+ thick (* 0.5 d))) 0))
    (ice-polar-xy p (list (+ thick (* 0.5 d)) 0)))
  (ssadd (entlast) s))

;;;************************************************************
;;;序  号：0057
;;;名  字：ice-AddDimlinear
;;;功  能：线型标注
;;;参  数：str -> 标注字符串，p0 -> 定位点，p1 -> 标注点1，p2 -> 标注点2
;;;返回值：
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-AddDimlinear(str p0 p1 p2)
  (entmake (list (cons 0 "DIMENSION")
		 (cons 100 "AcDbEntity")
		 (cons 100 "AcDbDimension")
		 (cons 10 p0)
		 (cons 70 33)
		 (cons 1 str)
		 (cons 100 "AcDbAlignedDimension")
		 (cons 13 p1)
		 (cons 14 p2))))

;;;************************************************************
;;;序  号：0056
;;;名  字：ice-polar-xy
;;;功  能：相对坐标定位点
;;;参  数：p -> 源点，lst -> 位移向量
;;;返回值：目标点
;;;思  路：
;;;备  注：
;;;示  例：
;;;************************************************************

(defun	ice-polar-xy(p lst)
  (polar (polar p 0 (car lst)) (* 0.5 pi) (cadr lst)))

;;;************************************************************
;;;序  号：0055
;;;名  字：ice-list->str-bydelimiter
;;;功  能：列表（元素为字符串）根据分隔符重组成字符串
;;;参  数：lst -> 列表，delimiter -> 分隔符
;;;返回值：str
;;;思  路：递归
;;;备  注：
;;;示  例：(ice-list->str-bydelimiter '("20" "17" "02" "17" "14" "39" "11" "86") "-")
;;;************************************************************

(defun	ice-list->str-bydelimiter(lst delimiter)
  (if (null (cdr lst))
    (car lst)
    (strcat (car lst) delimiter (ice-list->str-bydelimiter (cdr lst) delimiter))))

;;;************************************************************
;;;序  号：0054
;;;名  字：ice-str->list-byNum
;;;功  能：根据字符个数，字符串转列表
;;;参  数：str -> 字符串，n -> 个数
;;;返回值：列表
;;;思  路：递归
;;;备  注：
;;;示  例：(ice-str->list-bynum "2017021714391186" 2)
;;;************************************************************

(defun	ice-str->list-byNum(str n)
  (if (<= (strlen str) n)
    (list str)
    (cons (substr str 1 n) (ice-str->list-byNum (substr str (1+ n)) n))))

;;;************************************************************
;;;序  号：0053
;;;名  字：ice-AddDimStyle
;;;功  能：创建标注样式
;;;参  数：
;;;返回值：
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-AddDimstyle(name number)
  (vl-cmdf "dimstyle" "s" name)
  (setvar "dimlfac" 1) ;scale factory
  (setvar "dimtxsty" "松羽") ;标注样式字体
  (setvar "dimasz" (* 2.5 (/ number 5.0))) ;arrow size
  (setvar "dimtxt" (* 2.5 (/ number 5.0))) ;文字高度
  (setvar "dimexe" (* 1.25 (/ number 5.0))) ;Extend beyond dim lines 超出尺寸线距离
  (setvar "dimexo" (* 0.625 (/ number 5.0))) ;Offset from origin 偏离原点距离
  (setvar "dimclrd" 3) ;尺寸线颜色
  (setvar "dimclre" 3) ;尺寸界限颜色
  (setvar "dimclrt" 7) ;尺寸文字颜色
  (setvar "dimdec" 0)) ;小数位

;;;************************************************************
;;;序  号：0052
;;;名  字：ice-AddLayer
;;;功  能：创建图层
;;;参  数：name -> 图层名
;;;返回值：若已存在，则返回nil，不影响。
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-AddLayer(name)
  (entmake (list (cons 0 "layer")
		 (cons 100 "AcDbSymbolTableRecord")
		 (cons 100 "AcDbLayerTableRecord")
		 (cons 70 0)
		 (cons 2 name)))
  (tblobjname "layer" name))

;;;************************************************************
;;;序  号：0051
;;;名  字：ice-AddLineType
;;;功  能：创建线型
;;;参  数：name -> 名字，style -> 类型
;;;返回值：若已存在，则返回nil，不影响。
;;;思  路：
;;;备  注：(ice-AddLineType "Center"  "Center ____ _ ____ _ ____ _ ____ _ ____ _ ____"))
;;;************************************************************

(defun ice-AddLineType(name style)
  (entmake
    (list '(0 . "LTYPE")
	  '(100 . "AcDbSymbolTableRecord")
	  '(100 . "AcDbLinetypeTableRecord")
	  (cons 2 name)
	  (cons 3 style)
	  '(70 . 0)
	  '(73 . 2)
	  '(40 . 15.0)
	  '(49 . 10.0)
	  '(74 . 0)
	  '(49 . -5.0)
	  '(74 . 0))))

;;;************************************************************
;;;序  号：0050
;;;名  字：ice-AddTextStyle
;;;功  能：创建字体
;;;参  数：name -> 字体名自定义，style0 -> 类型0，style1 -> 类型1，w -> wide，h -> 高度
;;;返回值：若已存在，则返回nil，不影响。
;;;思  路：
;;;备  注：(ice-AddTextStyle "松羽" "isocp.shx" "gbcbig.shx" 0.8 0)
;;;************************************************************

(defun	ice-AddTextStyle(name style0 style1 w h)
  (entmake (list '(0 . "STYLE")
		 '(100 . "AcDbSymbolTableRecord")
		 '(100 . "AcDbTextStyleTableRecord")
		 (cons 2 name)
		 (cons 3 style0)
		 (cons 4 style1)
		 '(70 . 0)
		 (cons 40 h)
		 (cons 41 w))))

;;;************************************************************
;;;序  号：0049
;;;名  字：tan
;;;功  能：正切
;;;参  数：弧度
;;;返回值：弧度的正切
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	tan(x)
  (/ (sin x) (cos x)))

;;;************************************************************
;;;序  号：0048
;;;名  字：ice-get-color
;;;功  能：图元颜色取值
;;;参  数：obj -> 图元对象
;;;返回值：（0-256，0-byblock,256-bylayer）
;;;思  路：颜色组码较特殊，单独写了个子程序
;;;备  注：组码62较特殊
;;;************************************************************

(defun	ice-get-color(obj / c)
  (setq c (ice-get-dxfvalue (ice->ename obj) 62))
  (if c
    c
    256))

;;;************************************************************
;;;序  号：0047
;;;名  字：ice-Get-BlockChildList
;;;功  能：取得块内图元列表
;;;参  数：obj -> 宿主图元
;;;返回值：子图元列表
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-Get-BlockChildList(obj / n lst)
  (setq n (ice-Get-DxfValue obj 2))
  (setq lst (tblsearch "block" n))
  (setq obj (cdr (assoc -2 lst)))
  (setq lst nil)
  (setq lst (cons obj lst))
  (while (setq obj (entnext obj))
    (setq lst (cons obj lst)))
  lst)

;;;************************************************************
;;;序  号：0046
;;;名  字：ice-put-AttributeColor
;;;功  能：增强属性块指定标签图元的颜色赋值
;;;参  数：obj -> 宿主图元，label -> 标签，c -> 颜色
;;;返回值：组码表
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-put-AttributeColor(obj label c / obj0)
  (setq obj0 (car (vl-remove-if-not ''((x) (= label (ice-get-dxfvalue x 2))) (ice-AttributeList obj))))
  (ice-put-color obj0 c))

;;;************************************************************
;;;序  号：0045
;;;名  字：ice-get-AttributeValue
;;;功  能：增强属性块指定标签图元的指定属性取回
;;;参  数：obj -> 宿主图元，label -> 标签，n -> 子图元组码
;;;返回值：对应组码值
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-get-AttributeValue(obj label n / obj0)
  (setq obj0 (car (vl-remove-if-not ''((x) (= label (ice-get-dxfvalue x 2))) (ice-AttributeList obj))))
  (ice-get-dxfvalue obj0 n))

;;;************************************************************
;;;序  号：0044
;;;名  字：ice-put-AttributeValue
;;;功  能：增强属性块指定标签图元的指定属性赋值
;;;参  数：obj -> 宿主图元，label -> 标签，n -> 子图元组码，v -> 组码对应的值
;;;返回值：组码表
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-put-AttributeValue(obj label n v / obj0)
  (setq obj0 (car (vl-remove-if-not ''((x) (= label (ice-get-dxfvalue x 2))) (ice-AttributeList obj))))
  (ice-put-dxfvalue obj0 n v))

;;;************************************************************
;;;序  号：0043
;;;名  字：ice-AttributeList
;;;功  能：取属性块属性图元列表
;;;参  数：源属性块图元
;;;返回值：属性块属性图元列表
;;;思  路：entnext
;;;备  注：
;;;************************************************************

(defun	ice-AttributeList(obj / lst)
  (while (/= "SEQEND" (ice-get-dxfvalue 0 (entnext obj)))
    (setq obj (entnext obj)
	  lst (cons obj lst)))
  lst)

;;;************************************************************
;;;序  号：0042
;;;名  字：ice-pipe-weight
;;;功  能：计算管重量
;;;参  数：d -> 管外径mm，thick -> 管厚度mm，len -> 管长度mm，p -> 管密度(0.00785,0.00793)
;;;返回值：单位为kg的实数
;;;思  路：数学算法	
;;;备  注：
;;;************************************************************

(defun	ice-pipe-weight(d thick len p)
  (* 0.001 len p (* pi (- (expt (* 0.5 d) 2)
			  (expt (- (* 0.5 d) thick) 2)))))

;;;************************************************************
;;;序  号：0041
;;;名  字：ice-ExcelRead
;;;功  能：读取Excel
;;;参  数：File -> xlsx文件全路径，SheetName -> 表名，RangeStr -> 单元格范围
;;;返回值：变体列表
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-ExcelRead(file SheetName RangeStr / xls wbs wb shs sh rg www ttt)
  (vl-load-com)
  (setq xls (vlax-get-or-create-object "Excel.application"))
  (setq wbs (vlax-get-property xls 'Workbooks))
  (setq wb (vlax-invoke-method wbs "open" file))
  (setq shs (vlax-get-property wb "Sheets"))
  (setq sh (vlax-get-property shs "Item" SheetName))
  (setq rg (vlax-get-property sh "Range" RangeStr))
  (setq www (vlax-get-property rg 'value))
  (setq ttt (vlax-safearray->list (vlax-variant-value www)))
  (vlax-invoke-method wb "close")
  (vlax-invoke-method xls "quit")
  (vlax-release-object xls)
  ttt)

;;;************************************************************
;;;序  号：0040
;;;名  字：ice-LineTo
;;;功  能：点到曲线（包含直线），最短距离
;;;参  数：p -> 点，obj -> 曲线图元
;;;返回值：距离
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-LineTo(p obj)
  (distance p (vlax-curve-GetClosestPointTo (ice->obj obj) p t)))

;;;************************************************************
;;;序  号：0039
;;;名  字：ice-AddArc*
;;;功  能：绘制圆弧（利用多段线方式创造）
;;;参  数：p0 -> 起点，p1 -> 中点，a -> 凸度
;;;返回值：多段线（纯圆弧）对象，若a为0，则返回直线。
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-AddArc*(p0 p1 a)
  (entmake (list '(0 . "lwpolyline")
		 '(100 . "AcDbEntity")
		 '(100 . "AcDbPolyline")
		 (cons 90 (length lst))
		 (cons 10 p0)
		 (cons 42 a)
		 (cons 10 p1)))
  (entlast))

;;;************************************************************
;;;序  号：0038
;;;名  字：ice-AddArc
;;;功  能：绘制圆弧
;;;参  数：p -> 圆心，radius -> 半径，start -> 起始角度，stop -> 终止角度
;;;返回值：圆弧图元对象
;;;思  路：
;;;备  注：
;;;************************************************************

(defun	ice-AddArc(p radius start stop)
  (entmake
    (list '(0 . "arc")
	  '(100 . "AcDbEntity")
	  '(100 . "AcDbCircle")
	  (cons 10 p)
	  (cons 40 radius)
	  (cons 50 start)
	  (cons 51 stop)))
  (entlast))

;;;************************************************************
;;;序  号：0037
;;;名  字：ice-polyline-neighbor
;;;功  能：一点距离多段线最近点在多段线上的相邻点
;;;参  数：obj -> 多段线对象，p -> 点
;;;返回值：相邻的两点
;;;思  路：curve取得多段线角点是整实数，可利用之。
;;;备  注：
;;;************************************************************

(defun	ice-polyline-neighbor(obj p / lst)
  (setq obj (ice->obj obj)
	p (ice->point p)
	p (vlax-curve-getparamatpoint obj (vlax-curve-getclosestpointto obj p)))
  (mapcar ''((x) (vlax-curve-getpointatparam obj x)) (list (fix p) (1+ (fix p)))))

;;;************************************************************
;;;序  号：0036
;;;名  字：ice-time
;;;功  能：取系统时间
;;;参  数：None
;;;返回值：(("year" . "2016") ("month" . "12") ("day" . "22") ("hour" . "14") ("minute" . "13") ("second" . "14") ("millisecond" . "18"))
;;;思  路：利用系统函数封装
;;;备  注：
;;;************************************************************

(defun	ice-time(/ data)
  (vl-load-com)
  (setq data (rtos (getvar "cdate") 2 8)
	data (vl-string-subst "" "." data)
	data (ice-str->list-byNum data 2)
	data (cons (strcat (car data) (cadr data)) (cdr (cdr data))))
  (mapcar 'cons
	  '("year" "month" "day" "hour" "minute" "second" "millisecond")
	  data))

;;;************************************************************
;;;序  号：0035
;;;名  字：ice-substr
;;;功  能：字符串以指定间距切分为列表
;;;参  数：str -> 字符串，start -> 开始数（包含），stop -> 截止数（不包含），step -> 步长
;;;返回值：列表
;;;思  路：递归思想
;;;备  注：
;;;************************************************************

(defun	ice-substr(str start stop step)
  (if (< start stop)
    (cons (substr str start step) (ice-substr str (+ start step) stop step))))

;;;************************************************************
;;;序  号：0034
;;;名  字：ice-list-unique
;;;功  能：列表元素去重复
;;;参  数：lst -> 列表
;;;返回值：去重后列表
;;;思  路：递归思想
;;;备  注：
;;;************************************************************

(defun	ice-list-unique(lst)
  (if lst
    (cons (car lst) (ice-list-unique (vl-remove (car lst) (cdr lst))))))

;;;************************************************************
;;;序  号：0033
;;;名  字：ice-3d-direction
;;;功  能：判断三点顺时针
;;;参  数：三点的列表
;;;返回值：整数则顺时针，负数则逆时针
;;;思  路：向量叉积，作图即可理解
;;;备  注：
;;;************************************************************

(defun	ice-3p-direction(lst)
  (setq lst (mapcar 'ice->Point lst)
	lst (mapcar ''((x) (if (= 2 (length x)) x (list (car x) (cadr x)))) lst)
	lst (mapcar ''((x y) (mapcar '- x y)) lst (cdr lst)))
  (apply '- (mapcar '* (car lst) (reverse (last lst)))))

;;;************************************************************
;;;序  号：0032
;;;名  字：ice-sign
;;;功  能：取数的符号
;;;参  数：数
;;;返回值：符号，可用作函数
;;;思  路：利用系统函数构造
;;;备  注：
;;;************************************************************

(defun	ice-sign(x)
  (cond
    ((not (numberp x)) nil)
    (t (if (minusp x) - +))))

;;;************************************************************
;;;序  号：0031
;;;名  字：ice->Point
;;;功  能：点对象转点列表
;;;参  数：点对象
;;;返回值：点列表
;;;思  路：系统函数封装
;;;备  注：
;;;************************************************************

(defun	ice->Point(obj)
  (vl-load-com)
  (cond
    ((= 'list (type obj)) obj)
    ((= 'variant (type boj)) (vlax-safearray->list (vlax-variant-value obj)))
    (t nil)))

;;;************************************************************
;;;序  号：0030
;;;名  字：ice->Variant
;;;功  能：点对象转变体
;;;参  数：点对象
;;;返回值：变体
;;;思  路：系统函数封装
;;;备  注：
;;;************************************************************

(defun	ice->Variant(obj)
  (vl-load-com)
  (cond
    ((= 'variant (type obj)) obj)
    ((= 'list (type obj)) (vlax-3d-point obj))
    (t nil)))

;;;************************************************************
;;;序  号：0029
;;;名  字：ice-list-roll
;;;功  能：列表看做一个环，n为负左边滑行，n为正右边滑行。
;;;参  数：lst -> 列表，n -> 滑行距离
;;;返回值：新的list
;;;思  路：数据结构，环状
;;;备  注：
;;;************************************************************

(defun	ice-list-roll(lst n)
  (if (zerop n)
    lst
    (progn
      (if (minusp n)
	(ice-list-roll (append (cdr lst) (list (car lst))) (1+ n))
	(ice-list-roll (cons (last lst) (reverse (cdr (reverse lst)))) (1- n))))))

;;;************************************************************
;;;序  号：0028
;;;名  字：ice-InsertBlock
;;;功  能：插入图块
;;;参  数：p -> 插入点，n -> 块名，x -> x缩放，y -> y缩放，z -> z缩放，an -> 旋转角度
;;;返回值：暂无意义
;;;思  路：系统函数封装
;;;备  注：
;;;************************************************************

(defun	ice-InsertBlock(p n x y z an / myms)
  (vl-load-com)
  (setq myms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (vla-insertblock myms (vlax-3d-point p) n x y z an))

;;;************************************************************
;;;序  号：0027
;;;名  字：ice-dist
;;;功  能：distance扩展，点表距离
;;;参  数：lst -> 点列表
;;;返回值：系列点之间的距离列表
;;;思  路：扩展，经典思路
;;;备  注：
;;;************************************************************

(defun	ice-dist(lst)
  (mapcar 'distance lst (cdr lst)))

;;;************************************************************
;;;序  号：0026
;;;名  字：ice-mid*
;;;功  能：两点中点扩展，多点中点
;;;参  数：lst -> 点列表
;;;返回值：系列点中点列表
;;;思  路：扩展，经典思路
;;;备  注：
;;;************************************************************

(defun	ice-mid*(lst)
  (mapcar 'ice-mid lst (cdr lst)))

;;;************************************************************
;;;序  号：0025
;;;名  字：ice-mid
;;;功  能：两点中点
;;;参  数：p0 -> 点，p1 -> 点
;;;返回值：中点
;;;思  路：死代码
;;;备  注：
;;;************************************************************

(defun	ice-mid(p0 p1)
  (mapcar ''((x y) (* 0.5 (+ x y))) p0 p1))

;;;************************************************************
;;;序  号：0024
;;;名  字：ice-list->file
;;;功  能：字符串列表以行的形式写入文件
;;;参  数：lst -> 行字符串的列表，file -> 文件名，flag -> 是否追加（不追加则新建或覆盖）
;;;返回值：暂无意义
;;;思  路：写回文件，死代码
;;;备  注：
;;;************************************************************

(defun	ice-list->file(lst file flag / tmp f)
  (if flag
    (setq f (open file "a"))
    (setq f (open file "w")))
  (while (setq tmp (car lst))
    (write-line tmp f)
    (setq lst (cdr lst)))
  (close f))

;;;************************************************************
;;;序  号：0023
;;;名  字：ice-file->list
;;;功  能：文件以行为单位读取为表形式
;;;参  数：file -> 文件名，flag -> 是否忽略以分号开头的行
;;;返回值：行的列表
;;;思  路：封装read-line函数，根据通用性，也比较死
;;;备  注：
;;;************************************************************

(defun	ice-file->list(file flag / f tmp lst)
  (setq f (open file "r"))
  (while (setq tmp (read-line f))
    (setq lst (cons tmp lst)))
  (close f)
  (setq lst (reverse lst))
  (if flag
    (vl-remove-if ''((x) (= ";" (substr x 1 1))) lst)
    lst))

;;;************************************************************
;;;序  号：0022
;;;名  字：ice-reverse
;;;功  能：序列翻转
;;;参  数：暂时支持（列表，字符串）
;;;返回值：翻转后的对象
;;;思  路：封装通用reverse
;;;备  注：
;;;************************************************************

(defun	ice-reverse(obj)
  (cond
    ((= (type obj) 'list) (reverse obj))
    ((= (type obj) 'str) (apply 'strcat (reverse (ice-str->list obj))))
    (t nil)))

;;;************************************************************
;;;序  号：0021
;;;名  字：ice-str->list
;;;功  能：字符串转字符表
;;;参  数：字符串（支持ascii，unicode混用）
;;;返回值：字符表
;;;思  路：标准递归思路
;;;备  注：
;;;************************************************************

(defun	ice-str->list(str)
  (if (/= "" str)
    (if (> (ascii (substr str 1 1)) 128)
      (cons (substr str 1 2) (ice-str->list (substr str 3)))
      (cons (substr str 1 1) (ice-str->list (substr str 2))))))

;;;************************************************************
;;;序  号：0020
;;;名  字：ice-AddPipe
;;;功  能：绘制管道
;;;参  数：p0 -> 起点，p1 -> 中点，d -> 管外径，thick -> 管厚度， flag -> 是否绘制内壁
;;;返回值：暂未定
;;;思  路：死代码
;;;备  注：
;;;************************************************************

(defun	ice-AddPipe(p0 p1 d thick flag / obj)
  (ice-AddLine p0 p1)
  (setq obj (entlast))
  (ice-offset* obj (list (* 0.5 d) (- (* 0.5 d))))
  (if flag
    (ice-offset* obj (list (- thick (* 0.5 d)) (- (* 0.5 d) thick))))
  (ice-put-color obj 1))

;;;************************************************************
;;;序  号：0019
;;;名  字：ice-list->ss
;;;功  能：列表v选择集
;;;参  数：lst -> 图元列表
;;;返回值：选择集
;;;思  路：递归思路，循环实现
;;;备  注：
;;;************************************************************

(defun	ice-list->ss(lst / s)
  (setq s (ssadd))
  (while (car lst)
    (ssadd (car lst) s)
    (setq lst (cdr lst)))
  s)

;;;************************************************************
;;;序  号：0018
;;;名  字：ice-ss->list
;;;功  能：选择集->列表
;;;参  数：s -> 选择集
;;;返回值：图元列表
;;;思  路：标准递归思路
;;;备  注：源选择集会为空选择集
;;;************************************************************

(defun	ice-ss->list(s)
  (if (ssname s 0) (cons (ssname s 0) (ice-ss->list (ssdel (ssname s 0) s)))))

;;;************************************************************
;;;序  号：0017
;;;名  字：ice-offset*
;;;功  能：对象偏移扩展，遍历偏移
;;;参  数：obj -> 图元对象或对象列表，d -> 偏移距离或距离列表
;;;返回值：无意义
;;;思  路：加强版本的offset，遍历偏移
;;;备  注：
;;;************************************************************

(defun	ice-offset*(lst0 lst1)
  (if (atom lst0) (setq lst0 (list lst0)))
  (if (atom lst1) (setq lst1 (list lst1)))
  (mapcar ''((x)
	     (mapcar ''((y) (ice-offset x y)) lst1)) lst0))

;;;************************************************************
;;;序  号：0016
;;;名  字：ice-offset
;;;功  能：对象偏移
;;;参  数：obj -> 图元对象，d -> 偏移距离
;;;返回值：无意义
;;;思  路：和系统offset命令同等思路，封装了系统函数
;;;备  注：
;;;************************************************************

(defun	ice-offset(obj d / tmp)
  (vl-load-com)
  (if (numberp obj) (setq tmp obj obj d d tmp))
  (vla-offset (ice->obj obj) d))

;;;************************************************************
;;;序  号：0015
;;;名  字：ice-test
;;;功  能：测试用
;;;参  数：lst -> 点列表，h -> 文字高度，c -> 文字颜色
;;;返回值：无意义
;;;思  路：仅测试用，标识点的顺序和位置
;;;备  注：
;;;************************************************************

(defun	ice-test(lst h c)
  (mapcar ''((x y) (ice-AddText x (itoa y) h) (ice-put-color (entlast) c)) lst (ice-range 0 (length lst) 1)))

;;;************************************************************
;;;序  号：0014
;;;名  字：ice-put-color
;;;功  能：图元颜色赋值
;;;参  数：obj -> 图元对象，v -> 颜色值（0-256，0-byblock,256-bylayer）
;;;返回值：无意义
;;;思  路：颜色组码较特殊，单独写了个子程序
;;;备  注：组码62较特殊
;;;************************************************************

(defun	ice-Put-Color(obj v)
  (if (numberp obj) (setq tmp obj obj v v tmp))
  (entmod (append (entget (ice->ename obj)) (list (cons 62 v)))))

;;;************************************************************
;;;序  号：0013
;;;名  字：ice-curve-div
;;;功  能：曲线长度等分
;;;参  数：obj -> 曲线对象，n -> 等分数
;;;返回值：等分点列表
;;;思  路：利用系统函数等分曲线
;;;备  注：参数顺序无关性
;;;************************************************************

(defun	ice-curve-div(obj n / len tmp)
  (vl-load-com)
  (if (numberp obj) (setq tmp obj obj n n tmp))
  (setq len (ice-get-length obj))
  (mapcar ''((x) (vlax-curve-GetPointAtDist obj x)) (append (ice-range 0.0 len (/ len n 1.0)) (list len))))

;;;************************************************************
;;;序  号：0012
;;;名  字：ice-curve-vertex
;;;功  能：曲线两边端点
;;;参  数：obj -> 曲线对象
;;;返回值：两边端点的列表
;;;思  路：利用系统函数精简代码
;;;备  注：
;;;************************************************************

(defun	ice-curve-vertex(obj)
  (vl-load-com)
  (setq obj (ice->obj obj))
  (list (vlax-curve-GetStartPoint obj) (vlax-curve-GetEndPoint obj)))

;;;************************************************************
;;;序  号：0011
;;;名  字：ice-sublist
;;;功  能：顺序子列表生成，仿python
;;;参  数：lst -> 原列表，start -> 开始数（包含），stop -> 截止数（不包含），step -> 步长
;;;返回值：指定条件的子列表
;;;思  路：标准递归思路，列表长度一定范围，可能会递归深度越界
;;;备  注：
;;;************************************************************

(defun	ice-sublist*(lst start stop step)
  (if (< start stop) (cons (nth start lst) (ice-sublist* lst (+ start step) stop step))))

;;;************************************************************
;;;序  号：0010
;;;名  字：ice-range
;;;功  能：顺序列表生成，仿python
;;;参  数：start -> 开始数（包含），stop -> 截止数（不包含），step -> 步长
;;;返回值：指定条件的列表
;;;关键字：range，序列
;;;示  例：
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-range(start stop step / lst)
  (while (< start stop)
    (setq lst (cons start lst)
	  start (+ start step)))
  (reverse (vl-remove-if ''((x) (equal x stop 1e-8)) lst)))

;;;************************************************************
;;;序  号：0009
;;;名  字：ice-get-length
;;;功  能：取图元长度
;;;参  数：obj -> 图元对象
;;;返回值：长度
;;;关键字：长度
;;;示  例：
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-get-length(obj)
  (vl-load-com)
  (vlax-curve-GetDistAtParam obj (vlax-curve-GetEndParam obj)))

;;;************************************************************
;;;序  号：0008
;;;名  字：ice-AddMtext
;;;功  能：创建多行文字图元
;;;参  数：p -> 插入点，text -> 文字内容，h -> 文字高度
;;;返回值：多行文字图元
;;;关键字：多行文字
;;;示  例：
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-AddMtext(p text h)
  (entmake
    (list '(0 . "mtext")
	  '(100 . "AcDbEntity")
	  '(100 . "AcDbMText")
	  (cons 1 text)
	  (cons 10 p)
	  (cons 40 h)))
  (entlast))

;;;************************************************************
;;;序  号：0007
;;;名  字：ice-AddText
;;;功  能：创建单行文字图元
;;;参  数：p -> 插入点，text -> 文字内容，h -> 文字高度
;;;返回值：单行文字图元
;;;关键字：单行文字
;;;示  例：
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-AddText(p text h)
  (entmake
    (list '(0 . "text")
	  '(100 . "AcDbText")
	  (cons 1 text)
	  (cons 10 p)
	  (cons 40 h)))
  (entlast))

;;;************************************************************
;;;序  号：0006
;;;名  字：ice-AddPolyline
;;;功  能：创建多段线图元（纯直线）
;;;参  数：lst -> 点列表的列表
;;;返回值：多段线图元
;;;关键字：多段线，纯直线
;;;示  例：
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-AddPolyline(lst)
  (entmake (append
	     (list '(0 . "lwpolyline")
		   '(100 . "AcDbEntity")
		   '(100 . "AcDbPolyline")
		   (cons 90 (length lst)))
	     (mapcar ''((x) (cons 10 x)) lst)))
  (entlast))

;;;************************************************************
;;;序  号：0005
;;;名  字：ice-AddCircle
;;;功  能：创建圆图元
;;;参  数：p -> 圆心，r -> 半径
;;;返回值：圆图元
;;;关键字：圆
;;;示  例：
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-AddCircle(p r)
  (entmake
    (list '(0 . "circle")
	  (cons 10 p)
	  (cons 40 r)))
  (entlast))

;;;************************************************************
;;;序  号：0004
;;;名  字：ice-AddLine
;;;功  能：创建直线图元
;;;参  数：p0 -> 起点，p1 -> 终点
;;;返回值：直线图元
;;;关键字：直线
;;;示  例：
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-AddLine(p0 p1)
  (entmake
    (list '(0 . "line")
	  (cons 10 p0)
	  (cons 11 p1)))
  (entlast))

;;;************************************************************
;;;序  号：0003
;;;名  字：ice-put-dxfvalue
;;;功  能：赋予图元指定组码的值
;;;参  数：obj -> 图元，n -> 组码，v -> 值
;;;返回值：图元组码列表
;;;关键字：
;;;示  例：
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-put-dxfvalue(obj n v / tmp lst)
  (if (numberp obj) (setq tmp obj obj n n tmp))
  (setq obj (ice->ename obj)
	lst (entget obj))
  (entmod (subst (cons n v) (assoc n lst) lst)))

;;;************************************************************
;;;序  号：0002
;;;名  字：ice-get-dxfvalue
;;;功  能：取得图元指定组码的值
;;;参  数：obj -> 图元，n -> 组码
;;;返回值：value
;;;关键字：图元，组码，值，取得
;;;示  例：(ice-get-dxfvalue obj n)
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice-get-dxfvalue(obj n / tmp)
  (if (numberp obj) (setq tmp obj obj n n tmp))
  (setq obj (ice->ename obj))
  (cdr (assoc n (entget obj))))

;;;************************************************************
;;;序  号：0001
;;;名  字：ice->obj
;;;功  能：ename -> vla-object
;;;参  数：vla-object or ename
;;;返回值：vla-object or nil
;;;关键字：图元，对象
;;;示  例：(ice->obj obj)
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice->obj(obj)
  (vl-load-com)
  (cond
    ((= 'vla-object (type obj)) obj)
    ((= 'ename (type obj)) (vlax-ename->vla-object obj))))

;;;************************************************************
;;;序  号：0000
;;;名  字：ice->ename
;;;功  能：vla-object -> ename
;;;参  数：vla-object or ename
;;;返回值：ename or nil
;;;关键字：图元，对象
;;;示  例：(ice->ename obj)
;;;时  间：2017.03.14
;;;************************************************************

(defun	ice->ename(obj)
  (vl-load-com)
  (cond
    ((= 'ename (type obj)) obj)
    ((= 'vla-object (type obj)) (vlax-vla-object->ename obj))))