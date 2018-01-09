; ModuleID = 'runtime.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.2 = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1
@.str.3 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@stdin = external global %struct._IO_FILE*, align 8

; Function Attrs: nounwind uwtable
define void @printInt(i32 %n) #0 {
  %1 = alloca i32, align 4
  store i32 %n, i32* %1, align 4
  %2 = load i32, i32* %1, align 4
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %2)
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: nounwind uwtable
define void @printString(i8* %s) #0 {
  %1 = alloca i8*, align 8
  store i8* %s, i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0), i8* %2)
  ret void
}

; Function Attrs: nounwind uwtable
define void @error() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 1) #5
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: noreturn nounwind
declare void @exit(i32) #2

; Function Attrs: nounwind uwtable
define i32 @readInt() #0 {
  %n = alloca i32, align 4
  %result = alloca i32, align 4
  %c = alloca i8, align 1
  %1 = call i32 (i8*, ...) @__isoc99_scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i32 0, i32 0), i32* %n)
  store i32 %1, i32* %result, align 4
  %2 = load i32, i32* %result, align 4
  %3 = icmp eq i32 %2, -1
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  call void @error()
  br label %5

; <label>:5                                       ; preds = %4, %0
  br label %6

; <label>:6                                       ; preds = %22, %5
  %7 = load %struct._IO_FILE*, %struct._IO_FILE** @stdin, align 8
  %8 = call i32 @_IO_getc(%struct._IO_FILE* %7)
  %9 = trunc i32 %8 to i8
  store i8 %9, i8* %c, align 1
  %10 = sext i8 %9 to i32
  %11 = icmp eq i32 %10, 32
  br i1 %11, label %20, label %12

; <label>:12                                      ; preds = %6
  %13 = load i8, i8* %c, align 1
  %14 = sext i8 %13 to i32
  %15 = icmp eq i32 %14, 9
  br i1 %15, label %20, label %16

; <label>:16                                      ; preds = %12
  %17 = load i8, i8* %c, align 1
  %18 = sext i8 %17 to i32
  %19 = icmp eq i32 %18, 10
  br label %20

; <label>:20                                      ; preds = %16, %12, %6
  %21 = phi i1 [ true, %12 ], [ true, %6 ], [ %19, %16 ]
  br i1 %21, label %22, label %23

; <label>:22                                      ; preds = %20
  br label %6

; <label>:23                                      ; preds = %20
  %24 = load i8, i8* %c, align 1
  %25 = sext i8 %24 to i32
  %26 = load %struct._IO_FILE*, %struct._IO_FILE** @stdin, align 8
  %27 = call i32 @ungetc(i32 %25, %struct._IO_FILE* %26)
  %28 = load i32, i32* %n, align 4
  ret i32 %28
}

declare i32 @__isoc99_scanf(i8*, ...) #1

declare i32 @_IO_getc(%struct._IO_FILE*) #1

declare i32 @ungetc(i32, %struct._IO_FILE*) #1

; Function Attrs: nounwind uwtable
define i8* @readString() #0 {
  %line = alloca i8*, align 8
  %size = alloca i64, align 8
  store i8* null, i8** %line, align 8
  %1 = load %struct._IO_FILE*, %struct._IO_FILE** @stdin, align 8
  %2 = call i64 @getline(i8** %line, i64* %size, %struct._IO_FILE* %1)
  %3 = icmp eq i64 %2, -1
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  call void @error()
  br label %5

; <label>:5                                       ; preds = %4, %0
  %6 = load i8*, i8** %line, align 8
  %7 = call i64 @strlen(i8* %6) #6
  %8 = sub i64 %7, 1
  %9 = load i8*, i8** %line, align 8
  %10 = getelementptr inbounds i8, i8* %9, i64 %8
  store i8 0, i8* %10, align 1
  %11 = load i8*, i8** %line, align 8
  ret i8* %11
}

declare i64 @getline(i8**, i64*, %struct._IO_FILE*) #1

; Function Attrs: nounwind readonly
declare i64 @strlen(i8*) #3

; Function Attrs: nounwind uwtable
define i8* @concat(i8* %s1, i8* %s2) #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i8*, align 8
  %t = alloca i8*, align 8
  store i8* %s1, i8** %1, align 8
  store i8* %s2, i8** %2, align 8
  %3 = load i8*, i8** %1, align 8
  %4 = call i64 @strlen(i8* %3) #6
  %5 = load i8*, i8** %2, align 8
  %6 = call i64 @strlen(i8* %5) #6
  %7 = add i64 %4, %6
  %8 = add i64 %7, 1
  %9 = call noalias i8* @malloc(i64 %8) #7
  store i8* %9, i8** %t, align 8
  %10 = load i8*, i8** %t, align 8
  %11 = load i8*, i8** %1, align 8
  %12 = call i8* @strcpy(i8* %10, i8* %11) #7
  %13 = load i8*, i8** %2, align 8
  %14 = call i8* @strcat(i8* %12, i8* %13) #7
  ret i8* %14
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #4

; Function Attrs: nounwind
declare i8* @strcat(i8*, i8*) #4

; Function Attrs: nounwind
declare i8* @strcpy(i8*, i8*) #4

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { noreturn nounwind }
attributes #6 = { nounwind readonly }
attributes #7 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.7.0 (tags/RELEASE_370/final)"}
