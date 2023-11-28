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
    auto mainFunc = Function::create(FunctionType::get(Int32Type, {}), "main", module);
    auto bb = BasicBlock::create(module, "entry", mainFunc);
    builder->set_insert_point(bb);
    auto a = builder->create_alloca(Int32Type);
    auto i = builder->create_alloca(Int32Type);
    builder->create_store(ConstantInt::get(10, module), a);
    builder->create_store(ConstantInt::get(0, module), i);
    auto While = BasicBlock::create(module, "While", mainFunc);
    auto WhileLoop = BasicBlock::create(module, "WhileLoop", mainFunc);
    auto Return = BasicBlock::create(module, "Return", mainFunc);

    builder->create_br(While);
    builder->set_insert_point(While);
    auto load_i = builder->create_load(i);
    auto icmp = builder->create_icmp_lt(load_i, ConstantInt::get(10, module));
    builder->create_cond_br(icmp, WhileLoop, Return);
    builder->set_insert_point(WhileLoop);
    auto i_increment = builder->create_iadd(load_i, ConstantInt::get(1, module));
    builder->create_store(i_increment, i);
    auto load_a = builder->create_load(a);
    auto a_add_i = builder->create_iadd(load_a, i_increment);
    builder->create_store(a_add_i, a);
    builder->create_br(While);

    builder->set_insert_point(Return);
    load_a = builder->create_load(a);
    builder->create_ret(load_a);
    
    std::cout << module->print();
    delete module;
    delete builder;
    return 0;
}