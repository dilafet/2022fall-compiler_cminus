#include "BasicBlock.h"
#include "Constant.h"
#include "Function.h"
#include "IRBuilder.h"
#include "Module.h"
#include "Type.h"

#include <iostream>
#include <memory>

int main() {
    auto module = new Module("assign");
    auto builder = new IRBuilder(nullptr, module);
    Type *Int32Type = Type::get_int32_type(module);
    auto bb = BasicBlock::create(module, "entry", Function::create(FunctionType::get(Int32Type, {}), "main", module));
    builder->set_insert_point(bb);
    auto array_a = builder->create_alloca(ArrayType::get(Int32Type, 10));
    auto a0 = builder->create_gep(array_a, {ConstantInt::get(0, module), ConstantInt::get(0, module)});
    builder->create_store(ConstantInt::get(10, module), a0);
    auto load_a0 = builder->create_load(a0);
    auto a0_mul_2 = builder->create_imul(load_a0, ConstantInt::get(2, module)); 
    auto a1 = builder->create_gep(array_a, {ConstantInt::get(0, module), ConstantInt::get(1, module)});
    builder->create_store(a0_mul_2, a1);
    auto load_a1 = builder->create_load(a1);
    builder->create_ret(load_a1);
    std::cout << module->print();
    delete module;
    delete builder;
    return 0;
}