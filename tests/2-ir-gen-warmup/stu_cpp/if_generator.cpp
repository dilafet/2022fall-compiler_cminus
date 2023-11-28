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
    Type *FloatType = Type::get_float_type(module);

    auto mainFunc = Function::create(FunctionType::get(Int32Type, {}), "main", module);
    auto bb = BasicBlock::create(module, "entry", mainFunc);
    builder->set_insert_point(bb);

    auto a = builder->create_alloca(FloatType);
    auto return_val = builder->create_alloca(Int32Type);
    builder->create_store(ConstantInt::get(0, module), return_val);
    builder->create_store(ConstantFP::get(5.555, module), a);

    auto load_a = builder->create_load(a);
    auto fcmp = builder->create_fcmp_gt(load_a, ConstantFP::get(1.0, module));

    auto True = BasicBlock::create(module, "True", mainFunc);
    auto Return = BasicBlock::create(module, "Return", mainFunc);
    
    auto br = builder->create_cond_br(fcmp, True, Return);
    builder->set_insert_point(True);

    builder->create_store(ConstantInt::get(233, module), return_val);
    builder->create_br(Return);

    builder->set_insert_point(Return);
    auto load_return_val = builder->create_load(return_val);
    builder->create_ret(load_return_val);

    std::cout << module->print();
    delete module;
    delete builder;
    return 0;
}
