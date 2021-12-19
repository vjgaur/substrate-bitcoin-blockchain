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
