#include "BasicBlock.h"
#include "Constant.h"
#include "Function.h"
#include "IRBuilder.h"
#include "Module.h"
#include "Type.h"

#include <iostream>
#include <memory>

int main() {
    auto module = new Module("func");
    auto builder = new IRBuilder(nullptr, module);
    Type *Int32Type = Type::get_int32_type(module);
    
    //callee
    auto calleeFunc = Function::create(FunctionType::get(Int32Type, {Int32Type}), "callee", module);
    auto bb = BasicBlock::create(module, "entry", calleeFunc);
    builder->set_insert_point(bb);

    auto a = builder->create_alloca(Int32Type);
    auto arg = calleeFunc->arg_begin();
    builder->create_store(*arg, a);
    auto load_a = builder->create_load(a);
    auto mul = builder->create_imul(ConstantInt::get(2, module), load_a);
    builder->create_ret(mul);

    //main
    auto mainFunc = Function::create(FunctionType::get(Int32Type,{}), "main", module);
    bb = BasicBlock::create(module, "entry", mainFunc);
    builder->set_insert_point(bb);
    auto call = builder->create_call(calleeFunc, {ConstantInt::get(110, module)});
    builder->create_ret(call);

    std::cout << module->print();
    delete module;
    delete builder;
    return 0;
}
