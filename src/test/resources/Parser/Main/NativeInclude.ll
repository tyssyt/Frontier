%LlvmClass = type { i64 }

declare noalias i8* @malloc(i32)

define nonnull %LlvmClass* @CALLME(i64 %number) unnamed_addr {
entry:
  %malloc = tail call i8* @malloc(i32 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i32))
  %res = bitcast i8* %malloc to %LlvmClass*
  %GEP_number = getelementptr inbounds %LlvmClass, %LlvmClass* %res, i32 0, i32 0
  store i64 %number, i64* %GEP_number
  ret %LlvmClass* %res
}