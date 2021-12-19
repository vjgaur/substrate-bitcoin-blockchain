use super::Aura;
use codec::{Decode, Encode};
use frame_support::{
	decl_event, decl_module, decl_storage,
	dispatch::{DispatchResult, Vec},
	ensure,
};
#[cfg(feature = "std")]
use serde::{Deserialize, Serialize};
use sp_core::sr25519::{Public, Signature};
use sp_core::{H256, H512};
use sp_runtime::traits::{BlakeTwo256, Hash, SaturatedConversion};
use sp_runtime::transaction_validity::{TransactionLongevity, ValidTransaction};
use sp_std::collections::btree_map::BTreeMap;

pub trait Trait: system::Trait {
	type Event: From<Event> + Into<<Self as system::Trait>::Event>;
}

#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
#[derive(PatialEq, Eq, PartialOrd, Ord, Default, Clone, Encode, Decode, Hash, Debug)]
pub struct TransactionInput {
	pub outpoint: H256,  // a reference to a UTXO to be spent
	pub sigscript: H512, // proof that transaction owener is authorised to spend to refered UTXO and is als a proof that the entire transction is untamperred
}

pub type Value = u128;

#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
#[derive(PatialEq, Eq, PartialOrd, Ord, Default, Clone, Encode, Decode, Hash, Debug)]
pub struct TransactionOutput {
	pub value: Value, //reference associated with this UTXO
	pub pubkey: H256, // public key associated with this output . key of the Utxo's owner
}

#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
#[derive(PatialEq, Eq, PartialOrd, Ord, Default, Clone, Encode, Decode, Hash, Debug)]
pub struct Transaction {
	pub inputs: Vect<TransactionInput>,
	pub outputs: Vec<TransactionOutput>,
}

decl_storage! {
	trait Store for Module<T: Trait> as Utxo {

		UtxoStore build(|config: &GensisConfig|{
			config.genesis_utxos
				   .iter()
				   .cloned()
				   .map(|u| (BlakeTwo256::hash_of(&u),u))
				   .collect::<Vec<_>>()
		}): map hasher(identity) H256 => Option<TransactionOutput>;
	
		pub RewardTotal get (reward_total):Vallue;
	}
	add_extra_genesis {
		config(genesis_utxos): Vec<TransactionOutput>;
	}		
}


// External functions: callable by the end user
decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event() = default;

		pub fn spend(_origin, transaction: Transaction)=> DispatchResult{
			//1. Check if the transaction is valid

			let reward = Self::validate_transaction(&transaction)?;

			//2. If the tx is valid update the storage
			//item namely the UTXOStore to reflect the state transaction either new UTXOs that are created
			let reward :Value = 0;
			Self::update_storage(&transaction, reward)?;


			//3. Emitting Success Event signaling to everyone that our transction has succeeded
			Self::deposit_event(Event::TransactionSuccess(Transaction)) // deposit event writes an event into a particular blocks event records, it expects event emuration that you need to creat in decl_event! macro
			Ok(())
		}

		fn on_finalize(){
			let auth:Vec<_> = Aura::authorities().iter().map(|x|){
				let r: &Public = x.as_ref();
				r.0.into()
			}).collect();
			Self::disperse_reward(&auth);
		}

	}
}

decl_event! {
	pub enum Event {
		TransactionSuccess(Transaction),
	}
}

impl<T: Trait> Module<T> {
	

	pub fn get_simple_transaction(transaction: &Transaction)-> Vec<u8>{
		let mut trx = transaction.cloned();

		for input in trx.inputs.iter_mut(){
			input.sigscript = H512::zero();
		}
		trx.encode();

	}
	// Inputs and outputs are not empty
	// Total output value must not exceed total input value
	// New outputs do not collige with the existing ones
	// Replay attacks are not possible 
	     // The input UTXo is indeed signed by the owner
		 // Transactions are tamperproof



	//This validate function takes transaction reference to a transaction type object
	pub fn validate_transaction(transaction: &Transaction) -> Result<Value, &static str>{
	//Let's copy our list of security checks that we need to implement here.
		ensure!(!transactio  n.inputs.is_empty(),"no inputs")
		ensure!(!transaction.outputs.is_empty(),"no outputs")

	// Each input exists and is used exactly once 
		{
			let input_set: BTreeMap<_,()> = transaction.inputs.iter().map(|input| (input,())).collect();
			ensure! (input_set.len() == transaction.inputs.len(),"each input must only be used once");
		}
	// Each output is defined exactly  once and has nonzero value

		{
			let output_set: BTreeMap<_,()> = transaction.outputs.iter().map(|output| (output,())).collect();
			ensure! (output_set.len() == transaction.outputs.len(),"each output must be defined only once");
		}

		let simple_transaction = Self:: get_simple_transaction(transaction);
		for input in transaction.inputs.iter(){
			//Attempt to get this particular hash = get(&input.outpoint) from the hashmap UtxoStore and if that hash exist, name the return value (Utxo) as input_utxo
			if let  some(input_utxo) = <UtxoStore>::get(&input.outpoint) {
				ensure! (sp_io::sr25519_verify(
					&Signature::from_raw(*input.sigscript.as_fixed_bytes()),
					&simple_transaction,
					&Public::from_h256(input_utxo.pubkey)
				), "signature must be valid");
				total_input = total_input.checked_add(input_utxo.value).ok_or("input value overflow")?;
			}	
			else
			{


			}
		}
		for output in transaction.outputs.iter(){
			ensure!(output.value > 0, "output value must be nonzero");
			let hash = BlakeTwo256::hash_of(&(&transaction.encode(),output_index));
			output_index = output_index.checked_add(1).ok_or("output index overflow"))?;
			ensure!(!<UtxoStore>:: contains_key(hash), "output already exists");
			total_output = total_output.checked_add(output.value).ok_or("output value overflow")?;

		}
		ensure!(total_input >= total_output, "output value must not exceed input value");
		let reward = total_input.checked_sub(total_output).ok_or("reward underflow")?;

		Ok(reward)

	}
	
	fn update_storage(transaction: &Transaction, reward:Value) -> DispatchResult {
		
		let new_total = <RewardTotal>:get()
						.checked_add(reward)
						.ok_or("reward overflow")?;
		<RewardTotal>::put(new_total);
	        
		//1. Remove UTXO which is successfuly spent from UTXOStore
		for input in &transaction.inputs {
			//Itereating through transaction input to remove them from storage one by one
			<UtxoStore>::remove(input.outpoint); //outpoint is the key at which this old input UTXO is currently been stored
			                         /* Syntax <UtxoStore>:: is called a rust turbofish
									 and its a special rust syntax that lest you
									 specify the concrete type for a generic function,
									 method , struct of anythin else you want to use */
		}


		//2. Create New UTXO that declares in transaction output of this transaction and save this UTXOs to UTXOStore
		lwr mut index:u64 = 0;
		for output in &transaction.outputs {
			// is a vector of transaction outputs
			let hash = BlakeTwo256.hash_of(&(&transaction.encode(),index)); // Calculate the hash over the key for how each transaction output will be stored which will be a BlackeTwo256 hashing the refrence of output
			index = index.checked_add(1).ok_or("output index overflow")?;
			<UtxoStore>::insert(hash, output) // Inserting into UTXoStore new key => value pair
		}
		Ok(())
	}

	//Disper reward function is executed at the end of the every Unique Block
fn disperse_reward(authorities: &[H256]){
	//1.Divide the reward fairly & Calculate how much each validator is going to get
	 let reward = <RewardTotal>::take();
	 let share_value:Value = reward
	 .checked_div(authorities.len() as Value)
	 .ok_or("No authorities")
	 .unwrap();

	 if share_value == 0 { return } // if the reward is 0 then prevent blockchain from executing the coputation 

	 let remainder = reward // Calculating the remainder left after sharing the rewards among validatiors / authorities 
	 .checked_sub(shared_value*authorities.len() as Value)
	 .ok_or("Sub underflow")
	 .unwrap();

	 <ReawardTotal>:put(remainder as Value) // Remaider total left after shared rewards is saved as reward for the next block for validator 

	//2. Crearte UTxo per validator 
	for authority in authorities {
// Iterate and Create and write one Utxo containing the shared_value per authority
		let utxo = TransactionOutput {
			value:share_value,
			pubkey: *authority,
		};

		//Ensuring everytime when dispersing the reward that are containing the Utxos with Unique hashes 
		let hash = BlakeTwo256::hash_of(&(&utxo,
		<system::Module<T>>::block_number().staurated_into::<u64>())); // Making sure the Block_number is unique
		
		if !<UtxoStore>::contains_key(hash){
			<UtxoStore>::insert(hash,utxo);
		}

		//Finally saving the above newly generated Utxos with their respective hash  into UtxoStore

		if !<UtxoStore>::contains_key(hash){ // Checking if the UTxoStore  already contains the same key/hash or not
			<UtxoStore>::insert(hash,utxo); // Inset the hash in UtxoStore 
			sp_runtime::print("Transaction reward sent to ");
			sp_runtime::print((hash.as_fixed_bytes() as &[u8]); // Converting hash into printable bytes value and then converting that byte value into a reference to a vector of [u8] which is printable
		}
		else {
			sp_runtime::print("Transaction reward wasted due to hash collision ");

		}
	}


	//3.Write Utxo to utxoStore 
}

}

/// Tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use frame_support::{
		assert_err, assert_ok, impl_outer_origin, parameter_types, weights::Weight,
	};
	use sp_core::testing::{KeyStore, SR25519};
	use sp_core::traits::KeystoreExt;
	use sp_runtime::{testing::Header, traits::IdentityLookup, Perbill};

	impl_outer_origin! {
		pub enum Origin for Test {}
	}

	#[derive(Clone, Eq, PartialEq)]
	pub struct Test;
	parameter_types! {
			pub const BlockHashCount: u64 = 250;
			pub const MaximumBlockWeight: Weight = 1024;
			pub const MaximumBlockLength: u32 = 2 * 1024;
			pub const AvailableBlockRatio: Perbill = Perbill::from_percent(75);
	}
	impl system::Trait for Test {
		type Origin = Origin;
		type Call = ();
		type Index = u64;
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type AccountId = u64;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type Event = ();
		type BlockHashCount = BlockHashCount;
		type MaximumBlockWeight = MaximumBlockWeight;
		type MaximumBlockLength = MaximumBlockLength;
		type AvailableBlockRatio = AvailableBlockRatio;
		type Version = ();
		type ModuleToIndex = ();
		type AccountData = ();
		type OnNewAccount = ();
		type OnKilledAccount = ();
	}
	impl Trait for Test {
		type Event = ();
	}

	type Utxo = Module<Test>;
}
